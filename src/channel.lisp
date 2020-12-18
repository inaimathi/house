(in-package :house)

(defparameter *channels* (make-hash-table))

(defun subscribe! (channel sock)
  (push sock (gethash channel *channels*))
  nil)

(defun subscribers-of (channel)
  (gethash channel *channels*))

(defun make-sse (data &key id event retry)
  (make-instance 'sse :data data :id id :event event :retry retry))

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
