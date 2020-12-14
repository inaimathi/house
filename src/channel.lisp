(in-package :house)

(defparameter *channels* (make-hash-table))

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
		    (write! message (flex-stream sock))
		    (force-output (socket-stream sock))
		    sock)
	     collect it))))

(defmethod publish! ((channel symbol) (message string))
  (publish! channel (make-sse message)))
