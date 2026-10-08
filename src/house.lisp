(in-package :house)

(defun start (port &optional (host usocket:*wildcard-host*))
  (assert (integerp port))
  (serve (socket-listen host port :reuse-address t :element-type 'octet)))

(defun serve (server &key stop-p)
  "Run the event loop on an already-bound listener, closing it on exit.
STOP-P, when supplied, permits cooperative shutdown between requests. Keeping
the listener separate lets tests use port zero without a port-selection race."
  (let ((conns (make-hash-table)))
    (unwind-protect
	 (loop until (and stop-p (funcall stop-p))
               do (loop for ready in
                        (wait-for-input
                         (cons server (alexandria:hash-table-keys conns))
                         :ready-only t
                         :timeout (if stop-p 0.05 #-windows nil #+windows 5))
		  do (process-ready ready conns)))
      (flet ((kill-sock! (sock)
	       #+lispworks (loop for res = (socket-close sock)
			      until (or (null res) (eql -1 res)))
	       #-lispworks (loop while (socket-close sock))))
	(loop for c being the hash-keys of conns do (kill-sock! c))
	(kill-sock! server)))))

(defun flush-and-close! (sock)
  ;; usocket >= 0.8.4 closes SBCL socket streams with :ABORT T (to plug an fd
  ;; leak), which discards anything still in the output buffer. Flush first,
  ;; or every response vanishes and clients see an empty reply.
  (ignore-errors (finish-output (socket-stream sock)))
  (ignore-errors (socket-close sock)))

(defun -error! (err sock)
  (ignore-errors (write-response! err (flex-stream sock)))
  (flush-and-close! sock))

(defun process-ready (ready conns)
  (assert (hash-table-p conns))
  (etypecase ready
    (stream-server-usocket (setf (gethash (socket-accept ready :element-type 'octet) conns) nil))
    (stream-usocket
     (let* ((buf (or (gethash ready conns)
                     (setf (gethash ready conns) (make-buffer (socket-stream ready)))))
            (status (buffer! buf)))
       (case status
         (:eof
          (remhash ready conns)
          (ignore-errors (socket-close ready)))
         (:bad
          (remhash ready conns)
          (-error! +400+ ready))
         (t
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
		    (handler-case
			(progn
			  (when (buffer-contents buf)
			    (setf (parameters (buffer-request buf))
				  (nconc (handler-case (parse-buffer buf)
					   ;; e.g. malformed JSON: the client's fault
					   (error () (error 'http-assertion-error :assertion :body)))
					 (parameters (buffer-request buf)))))
			  (handle-request! ready (buffer-request buf)))
		      (http-assertion-error () (-error! +400+ ready))
		      ;; This used to catch (AND (NOT WARNING) (NOT SIMPLE-ERROR)), so an
		      ;; (ERROR "...") in a handler escaped and took the server down.
		      (error () (-error! +500+ ready))))))))))))

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
	    (write-response! resp stream req)
	    (if (handler-entry-closing? handler)
		(flush-and-close! sock)
		(force-output stream))))
	(-error! +404+ sock))))
