(in-package :house)

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

(defun -error! (err sock)
  (ignore-errors
    (write-response! err (flex-stream sock))
    (socket-close sock)))

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
		    (-error! +413+ ready)
		    (remhash ready conns))
		   ((or too-old? too-needy?)
		    (-error! +400+ ready)
		    (remhash ready conns))
		   ((and (buffer-request buf) (zerop (buffer-expecting buf)))
		    (remhash ready conns)
		    (when (buffer-contents buf)
		      (setf (parameters (buffer-request buf)) (nconc (parse-buffer buf) (parameters (buffer-request buf)))))
		    (handler-case
			(handle-request! ready (buffer-request buf))
		      (http-assertion-error () (-error! +400+ ready))
		      #-CCL((and (not warning)
			     (not simple-error)) (e)
			     (-error! +500+ ready))
		      #+CCL(error (e)
			     (-error! +500+ ready)))))))))))

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
	(-error! +404+ sock))))
