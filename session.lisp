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

(defun new-session! ()
  (let ((session (make-instance 'session :token (new-session-token))))
    (setf (gethash (token session) *sessions*) session)
    (loop for hook in *new-session-hook*
	 do (funcall hook session))
    session))

(defun get-session! (token)
  (awhen (gethash token *sessions*)
    (setf (last-poked it) (get-universal-time))
    it))
