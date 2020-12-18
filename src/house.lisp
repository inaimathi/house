(in-package :house)

;;;;;;;;;; Function definitions
;;; The basic structure of the server is
; buffering-listen -> parse -> session-lookup -> handle -> channel

;;;;; Buffer/listen-related
(defun start (port &optional (host usocket:*wildcard-host*))
  (assert (integerp port))
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

(defun process-ready (ready conns)
  (assert (hash-table-p conns))
  (etypecase ready
    (stream-server-usocket (setf (gethash (socket-accept ready :element-type 'octet) conns) nil))
    (stream-usocket
     (let ((buf (or (gethash ready conns) (setf (gethash ready conns) (make-buffer (flex-stream ready))))))
       (if (eq :eof (buffer! buf))
	   (ignore-errors
	    (remhash ready conns)
	    (socket-close ready))
	   (let ((too-big? (> (buffer-total-buffered buf) +max-request-size+))
		 (too-old? (> (- (get-universal-time) (buffer-started buf)) +max-request-age+))
		 (too-needy? (> (buffer-tries buf) +max-buffer-tries+)))
	     (cond (too-big?
		    (error! +413+ ready)
		    (remhash ready conns))
		   ((or too-old? too-needy?)
		    (error! +400+ ready)
		    (remhash ready conns))
		   ((and (buffer-request buf) (zerop (expecting buf)))
		    (remhash ready conns)
		    (when (buffer-contents buf)
		      (setf (parameters (buffer-request buf)) (nconc (parse-buffer buf) (parameters (request buf)))))
		    (handler-case
			(handle-request! ready (buffer-request buf))
		      (http-assertion-error () (error! +400+ ready))
		      #-CCL((and (not warning)
			     (not simple-error)) (e)
			     (error! +500+ ready))
		      #+CCL(error (e)
			     (error! +500+ ready)))))))))))

;;;;; Parse-related
(defun parse-param-string (params)
  (loop for pair in (split "&" params)
	for (name val) = (split "=" pair)
	collect (cons (->keyword name) (or val ""))))

(defun parse-cookies (cookie)
  (assert (stringp cookie))
  (loop for c in (split "; " cookie) for s = (split "=" c)
     if (and (string= "name" (first s)) (second s)) collect (second s)
     else collect c))

(defun parse-request-string (str)
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
	(setf (parameters req) (parse-param-string parameters))
	(values req expecting)))))

;;;;; Handling requests
(defun handle-request! (sock req)
  (multiple-value-bind (handler path-params) (find-handler (http-method req) (resource req))
    (setf (parameters req) (append (parameters req) path-params))
    (if handler
	(let* ((check? (and (session-tokens req)
			    (loop for tok in (session-tokens req)
			       for s = (get-session! tok)
			       when s do (return s))))
	       (sess (or check? (new-session!))))
	  (setf (session req) sess
		(socket-of req) sock)
	  (let ((resp (funcall (handler-entry-fn handler) req))
		(stream (flex-stream sock)))
	    (setf (cookie resp) (unless check? (token sess)))
	    (write-response! resp stream)
	    (crlf stream)
	    (if (handler-entry-closing? handler)
		(socket-close sock)
		(progn
		  (force-output stream)
		  (write-sse!
		   (make-instance
		    'sse :data (json:encode-json-to-string
				'((:status . :ok)
				  (:message . "Listening..."))))
		   stream)
		  (force-output stream)))))
	(error! +404+ sock))))

(defun write-response! (res stream)
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

(defun error! (err sock)
  (ignore-errors
    (write-response! err (flex-stream sock))
    (socket-close sock)))
