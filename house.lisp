;; house.lisp
(in-package :house)

;;;;;;;;;; System tables
(defparameter *channels* (make-hash-table))

;;;;;;;;;; Function definitions
;;; The basic structure of the server is
; buffering-listen -> parse -> session-lookup -> handle -> channel

;;;;; Buffer/listen-related
(defmethod start ((port integer) &optional (host usocket:*wildcard-host*))
  (let ((server (socket-listen host port :reuse-address t :element-type 'octet))
	(conns (make-hash-table)))
    (unwind-protect
	 (loop (loop for ready in
		    #-windows(wait-for-input (cons server (alexandria:hash-table-keys conns)) :ready-only t)
		    #+windows(wait-for-input (cons server (alexandria:hash-table-keys conns)) :ready-only t :timeout 5)
		  do (process-ready ready conns)))
      (flet ((kill-sock! (sock)
	       #+lispworks (loop for res = (socket-close sock)
			      until (or (null res) (eql -1 res)))
	       #-lispworks (loop while (socket-close sock))))
	(loop for c being the hash-keys of conns do (kill-sock! c))
	(kill-sock! server)))))

(defmethod process-ready ((ready stream-server-usocket) (conns hash-table))
  (setf (gethash (socket-accept ready :element-type 'octet) conns) nil))

(defmethod process-ready ((ready stream-usocket) (conns hash-table))
  (let ((buf (or (gethash ready conns) (setf (gethash ready conns) (make-instance 'buffer :bi-stream (flex-stream ready))))))
    (if (eq :eof (buffer! buf))
	(ignore-errors
	  (remhash ready conns)
	  (socket-close ready))
	(let ((too-big? (> (total-buffered buf) +max-request-size+))
	      (too-old? (> (- (get-universal-time) (started buf)) +max-request-age+))
	      (too-needy? (> (tries buf) +max-buffer-tries+)))
	  (cond (too-big?
		 (error! +413+ ready)
		 (remhash ready conns))
		((or too-old? too-needy?)
		 (error! +400+ ready)
		 (remhash ready conns))
		((and (request buf) (zerop (expecting buf)))
		 (remhash ready conns)
		 (when (contents buf)
		   (setf (parameters (request buf))
			 (nconc (parse buf) (parameters (request buf)))))	     
		 (handler-case
		     (handle-request ready (request buf))
		   (http-assertion-error () (error! +400+ ready))
		   #-CCL((and (not warning)
			  (not simple-error)) (e)
			  (error! +500+ ready e))
		   #+CCL(error (e) 
			  (error! +500+ ready e)))))))))

(defun line-terminated? (lst)
  (starts-with-subseq 
   #-windows(list #\linefeed #\return #\linefeed #\return)
   #+windows(list #\newline #\newline)
   lst))

(defmethod buffer! ((buffer buffer))
  (handler-case
      (let ((stream (bi-stream buffer)))
	(incf (tries buffer))
	(loop for char = (read-char-no-hang stream)
	   until (or (null char) (eql :eof char))
	   do (push char (contents buffer))
	   do (incf (total-buffered buffer))
	   when (request buffer) do (decf (expecting buffer))
	   when (line-terminated? (contents buffer))
	   do (multiple-value-bind (parsed expecting) (parse buffer)
		(setf (request buffer) parsed
		      (expecting buffer) expecting
		      (contents buffer) nil)
		(return char))
	   when (> (total-buffered buffer) +max-request-size+) return char
	   finally (return char)))
    (error () :eof)))

;;;;; Parse-related
(defmethod parse-params ((params null)) nil)
(defmethod parse-params ((params string))
  (loop for pair in (split "&" params)
     for (name val) = (split "=" pair)
     collect (cons (->keyword name) (or val ""))))

(defmethod parse ((str string))
  (let ((lines (split "\\r?\\n" str))
	(expecting 0))
    (destructuring-bind (req-type path http-version) (split " " (pop lines))
      (declare (ignore req-type))
      (assert-http (string= http-version "HTTP/1.1"))
      (let* ((path-pieces (split "\\?" path))
	     (resource (first path-pieces))
	     (parameters (second path-pieces))
	     (req (make-instance 'request :resource resource)))
	(loop for header = (pop lines) for (name value) = (split ": " header)
	   until (null name)
	   for n = (->keyword name)
	   if (eq n :cookie) do (setf (session-tokens req) (split "; " value))
	   else if (eq n :content-length) do (setf expecting (parse-integer value))
	   else do (push (cons n value) (headers req)))
	(setf (parameters req) (parse-params parameters))
	(values req expecting)))))

(defmethod parse ((buf buffer))
  (let ((str (coerce (reverse (contents buf)) 'string)))
    (if (request buf)
	(parse-params str)
	(parse str))))

;;;;; Handling requests
(defmethod handle-request ((sock usocket) (req request))
  (aif (lookup (resource req) *handlers*)
       (let* ((check? (and (session-tokens req)
			   (loop for tok in (session-tokens req)
			      for s = (get-session! tok)
			      when s do (return s))))
	      (sess (aif check? it (new-session!))))
	 (funcall it sock check? sess (parameters req)))
       (error! +404+ sock)))

(defun crlf (&optional (stream *standard-output*))
  (write-char #\return stream)
  (write-char #\linefeed stream)
  (values))

(defmethod write! ((res response) (sock usocket))
  (let ((stream (flex-stream sock)))
    (flet ((write-ln (&rest sequences)
	     (mapc (lambda (seq) (write-sequence seq stream)) sequences)
	     (crlf stream)))
      (write-ln "HTTP/1.1 " (response-code res))  
      (write-ln "Content-Type: " (content-type res) "; charset=" (charset res))
      (write-ln "Cache-Control: no-cache, no-store, must-revalidate")
      (awhen (cookie res)
	(write-ln "Set-Cookie: " it))
      (awhen (location res)
	(write-ln "Location: " it))
      (when (keep-alive? res) 
	(write-ln "Connection: keep-alive")
	(write-ln "Expires: Thu, 01 Jan 1970 00:00:01 GMT"))
      (awhen (body res)
	(write-ln "Content-Length: " (write-to-string (length it)))
	#-windows(crlf stream)
	#+windows(format stream "~%")
	(write-ln it))
      (values))))

(defmethod write! ((res sse) (sock usocket))
  (let ((stream (flex-stream sock)))
    (format stream "~@[id: ~a~%~]~@[event: ~a~%~]~@[retry: ~a~%~]data: ~a~%~%"
	    (id res) (event res) (retry res) (data res))))

(defmethod error! ((err response) (sock usocket) &optional instance)
  (declare (ignorable instance))
  (ignore-errors 
    (write! err sock)
    (socket-close sock)))

;;;;; Channel-related
(defmethod subscribe! ((channel symbol) (sock usocket))
  (push sock (lookup channel *channels*))
  nil)

(defun make-sse (data &key id event retry)
  (make-instance 'sse :data data :id id :event event :retry retry))

(defmethod publish! ((channel symbol) (message sse))
  (awhen (lookup channel *channels*)
    (setf (lookup channel *channels*)
	  (loop for sock in it
	     when (ignore-errors 
		    (write! message sock)
		    (force-output (socket-stream sock))
		    sock)
	     collect it))))

(defmethod publish! ((channel symbol) (message string))
  (publish! channel (make-sse message)))
