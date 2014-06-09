(in-package #:house)

(defparameter *sessions* (make-hash-table :test 'equal))
(defparameter *new-session-hook* nil)

(defmacro new-session-hook! (&body body)
  `(push (lambda (session) ,@body)
	 *new-session-hook*))

(defun clear-session-hooks! ()
  (setf *new-session-hook* nil))

(let ((gen nil))
  (defun new-session-token! ()
    (unless gen 
      (setf gen (session-token:make-generator 
		 :token-length 64
		 :initial-seed #-win32 (isaac:init-kernel-seed) #+win32 (init-common-lisp-random-seed))))
    #+win32 (warn "Running on Windows; using insecure session tokens")
    (funcall gen)))

(let ((session-count 0))
  (defun new-session! ()
    (when (>= (incf session-count) +clean-sessions-every+)
      (setf session-count 0)
      (clean-sessions!))
    (let ((session (make-instance 'session :token (new-session-token!))))
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
