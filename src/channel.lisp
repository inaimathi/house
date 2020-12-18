(in-package :house)

(defparameter *channels* (make-hash-table))

(defstruct (sse (:constructor make-sse (data &key id event retry)))
  (id nil) (event nil) (retry nil)
  (data (error "an SSE must have :data") :type string))

(defun subscribe! (channel sock)
  (push sock (gethash channel *channels*))
  nil)

(defun subscribers-of (channel)
  (gethash channel *channels*))

(defun write-sse! (res stream)
  (format stream "~@[id: ~a~%~]~@[event: ~a~%~]~@[retry: ~a~%~]data: ~a~%~%"
	  (ss-id res) (sse-event res) (sse-retry res) (sse-data res)))

(defun publish! (channel msg)
  (let ((message (etypecase msg
		   (sse msg)
		   (string (make-sse msg)))))
    (when (subscribers-of channel)
      (setf (gethash channel *channels*)
	    (loop for sock in (subscribers-of channel)
		  when (ignore-errors
			(write-sse! message (flex-stream sock))
			(force-output (socket-stream sock))
			sock)
		    collect it)))))
