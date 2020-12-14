(in-package :house)

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
		     (handle-request! ready (request buf))
		   (http-assertion-error () (error! +400+ ready))
		   #-CCL((and (not warning)
			  (not simple-error)) (e)
			  (error! +500+ ready e))
		   #+CCL(error (e)
			  (error! +500+ ready e)))))))))

(defun line-terminated? (lst)
  (starts-with-subseq
   #-windows'(#\linefeed #\return #\linefeed #\return)
   #+windows'(#\newline #\newline)
   lst))

(defun buffer! (buffer)
  (handler-case
      (let ((stream (bi-stream buffer)))
	(incf (tries buffer))
	(loop for char = (read-char-no-hang stream)
	   until (or (null char) (eql :eof char))
	   do (push char (contents buffer))
	   do (incf (total-buffered buffer))
	   when (request buffer) do (decf (expecting buffer))
	   when (and #-windows(char= char #\linefeed)
		     #+windows(char= char #\newline)
		 (line-terminated? (contents buffer)))
	   do (multiple-value-bind (parsed expecting) (parse buffer)
		(setf (request buffer) parsed
		      (expecting buffer) expecting
		      (contents buffer) nil)
		(return char))
	   when (> (total-buffered buffer) +max-request-size+) return char
	   finally (return char)))
    (error () :eof)))

;;;;; Parse-related
(defmethod parse-params (content-type (params null)) nil)
(defmethod parse-params (content-type (params string))
  (loop for pair in (split "&" params)
     for (name val) = (split "=" pair)
     collect (cons (->keyword name) (or val ""))))

(defmethod parse-params ((content-type (eql :application/json)) (params string))
  (cl-json:decode-json-from-string params))

(defmethod parse-cookies ((cookie string))
  (loop for c in (split "; " cookie) for s = (split "=" c)
     if (and (string= "name" (first s)) (second s)) collect (second s)
     else collect c))

(defmethod parse ((str string))
  (let ((lines (split "\\r?\\n" str))
	(expecting 0))
    (destructuring-bind (http-method path http-version) (split " " (pop lines))
      (assert-http (string= http-version "HTTP/1.1"))
      (let* ((path-pieces (split "\\?" path))
	     (resource (first path-pieces))
	     (parameters (second path-pieces))
	     (req (make-instance 'request :resource resource :http-method (->keyword http-method))))
	(loop for header = (pop lines) for (name value) = (split ": " header)
	   until (null name)
	   for n = (->keyword name)
	   if (eq n :cookie) do (setf (session-tokens req) (parse-cookies value))
	   else if (eq n :content-length) do (setf expecting (parse-integer value))
	   else do (push (cons n value) (headers req)))
	(setf (parameters req) (parse-params nil parameters))
	(values req expecting)))))

(defmethod parse ((buf buffer))
  (let ((str (coerce (reverse (contents buf)) 'string)))
    (if (request buf)
	(parse-params
	 (->keyword (cdr (assoc :content-type (headers (request buf)))))
	 str)
	(parse str))))

;;;;; Handling requests
(defmethod handle-request! ((sock usocket) (req request))
  (multiple-value-bind (handler path-params) (find-handler (http-method req) (resource req))
    (setf (parameters req) (append (parameters req) path-params))
    (if handler
	(let* ((check? (and (session-tokens req)
			    (loop for tok in (session-tokens req)
			       for s = (get-session! tok)
			       when s do (return s))))
	       (sess (or check? (new-session!))))
	  (funcall handler sock check? sess req))
	(error! +404+ sock))))

(defun crlf (&optional (stream *standard-output*))
  (write-char #\return stream)
  (write-char #\linefeed stream)
  (values))

(defun write-ln (stream &rest sequences)
  (declare (optimize space speed))
  (dolist (s sequences) (write-sequence s stream))
  (crlf stream))

(defmethod write! ((res response) (stream stream))
  (write-ln stream "HTTP/1.1 " (response-code res))
  (write-ln stream "Content-Type: " (content-type res) "; charset=" (charset res))
  (loop for (name . value) in (headers res)
     when (not (member
		name '("Content-Type" "Set-Cookie" "Location" "Connection" "Expires" "Content-Length")
		:test-not #'string/=))
     do (write-ln stream name ": " value))
  (awhen (cookie res)
    (if (null *cookie-domains*)
	(write-ln stream "Set-Cookie: name=" it)
	(loop for d in *cookie-domains*
	   do (write-ln stream "Set-Cookie: name=" it "; domain=" d))))
  (awhen (location res)
    (write-ln stream "Location: " it))
  (when (keep-alive? res)
    (write-ln stream "Connection: keep-alive")
    (write-ln stream "Expires: Thu, 01 Jan 1970 00:00:01 GMT"))
  (awhen (body res)
    (write-ln stream "Content-Length: " (write-to-string (length it)))
    #-windows(crlf stream)
    #+windows(format stream "~%")
    (write-ln stream it))
  (values))

(defmethod write! ((res sse) (stream stream))
  (format stream "~@[id: ~a~%~]~@[event: ~a~%~]~@[retry: ~a~%~]data: ~a~%~%"
	  (id res) (event res) (retry res) (data res)))

(defmethod error! ((err response) (sock usocket) &optional instance)
  (declare (ignorable instance))
  (ignore-errors
    (write! err (flex-stream sock))
    (socket-close sock)))
