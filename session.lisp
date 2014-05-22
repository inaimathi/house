(in-package #:house)

(defparameter *sessions* (make-hash-table :test 'equal))
(defparameter *new-session-hook* nil)

(defmacro new-session-hook! (&body body)
  `(push (lambda (session) ,@body)
	 *new-session-hook*))

(defun clear-session-hooks! ()
  (setf *new-session-hook* nil))

(defmacro raw-token ()
  (let ((path (make-pathname :directory '(:absolute "dev") :name "urandom")))
    (if (cl-fad:file-exists-p path)
	`(cl-base64:usb8-array-to-base64-string
	  (with-open-file (s ,path :element-type '(unsigned-byte 8))
	    (make-array 32 :initial-contents (loop repeat 32 collect (read-byte s)))))
	`(progn (warn "/dev/urandom not found; using insecure session tokens")
		(coerce 
		 (loop with chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
		    repeat 32 collect (aref chars (random (length chars))))
		 'string)))))

(defun new-session-token ()
  (concatenate 'string "session=" (raw-token)))

(let ((session-count 0))
  (defun new-session! ()
    (when (>= (incf session-count) +clean-sessions-every+)
      (setf session-count 0)
      (clean-sessions!))
    (let ((session (make-instance 'session :token (new-session-token))))
      (setf (gethash (token session) *sessions*) session)
      (loop for hook in *new-session-hook*
	 do (funcall hook session))
      session)))

(defun get-session! (token)
  (awhen (gethash token *sessions*)
    (if (idling? it)
	(remhash token *sessions*)
	(poke! it))))

(defun clean-sessions! ()
  (loop for k being the hash-keys of *sessions*
     for v being the hash-values of *sessions*
     when (idling? v) do (remhash k *sessions*)))

(defmethod idling? ((sess session))
  (> (- (get-universal-time) (last-poked sess)) +max-session-idle+))

(defmethod poke! ((sess session))
  (setf (last-poked sess) (get-universal-time))
  sess)
