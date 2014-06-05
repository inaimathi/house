(in-package #:house)

(defparameter *sessions* (make-hash-table :test 'equal))
(defparameter *new-session-hook* nil)

(defmacro new-session-hook! (&body body)
  `(push (lambda (session) ,@body)
	 *new-session-hook*))

(defun clear-session-hooks! ()
  (setf *new-session-hook* nil))

(let* ((ctx nil)
       (alphabet (char-range (#\a :to #\z #\A :to #\Z #\0 :to #\9)))
       (token-length 64)
       (next-pow (next-power-of-two (length alphabet))))
  (defun next-char ()
    (loop for b = (rand-bits ctx next-pow) until (> (length alphabet) b)
       finally (return b)))
  
  (defun new-session-token! ()
    (unless ctx
      (setf ctx 
	    #-win32 (init-kernel-seed)
	    #+win32 (progn (warn "Running on Windows; using insecure session tokens")
			   (init-common-lisp-random-seed))))
    (let ((buf (make-string token-length)))
      (loop for i from 0 repeat token-length
	 do (setf (aref buf i) (aref alphabet (next-char))))
      buf)))

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
