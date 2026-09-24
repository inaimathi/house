(in-package #:house)

(defparameter *sessions* (make-hash-table :test 'equal :size 2000))
(defparameter *new-session-hook* nil)

(defclass session ()
  ((started :reader started :initform (get-universal-time))
   (last-poked :initform (get-universal-time))
   (token :reader token :initarg :token)
   (session-values :reader session-values :initform (make-hash-table :test 'equal))))

(defun new-session-hook! (callback)
  (push callback *new-session-hook*))

(defun clear-session-hooks! ()
  (setf *new-session-hook* nil))

(defparameter +token-alphabet+
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
(defparameter +token-length+ 64)

(let ((urandom nil))
  (defun random-octets (n)
    (let ((buf (make-array n :element-type 'octet)))
      #-windows
      (progn
        (unless (and urandom (open-stream-p urandom))
          (setf urandom (open "/dev/urandom" :element-type 'octet)))
        (read-sequence buf urandom))
      #+windows
      (progn
        (warn "Running on Windows; using insecure session tokens")
        (dotimes (i n) (setf (aref buf i) (random 256))))
      buf)))

(defun new-session-token! ()
  (let ((len (length +token-alphabet+))
        (out (make-string +token-length+))
        (i 0))
    ;; Rejection sampling: 248 = 4*62, so accepted octets map uniformly.
    (loop while (< i +token-length+)
          do (loop for b across (random-octets (* 2 +token-length+))
                   while (< i +token-length+)
                   when (< b (* len (floor 256 len)))
                     do (setf (char out i) (char +token-alphabet+ (mod b len)))
                        (incf i)))
    out))

(defun new-session! ()
  (when (zerop (random +clean-sessions-every+))
    (clean-sessions!))
  (let ((session (make-instance 'session :token (new-session-token!))))
    (setf (gethash (token session) *sessions*) session)
    (loop for hook in *new-session-hook*
	  do (funcall hook session))
    session))

(defun idling? (sess)
  (> (- (get-universal-time) (slot-value sess 'last-poked)) +max-session-idle+))

(defun get-session! (token)
  (awhen (gethash token *sessions*)
    (if (idling? it)
	(progn (remhash token *sessions*) nil)
	(poke! it))))

(defun clean-sessions! ()
  (loop for k being the hash-keys of *sessions*
	for v being the hash-values of *sessions*
	when (idling? v) do (remhash k *sessions*)))

(defun poke! (sess)
  (setf (slot-value sess 'last-poked) (get-universal-time))
  sess)
