(in-package #:house)

;;;;;;;;;; Parameter type parsing.
(defun process-uri (uri)
  (etypecase uri
    (string (split-at #\/ (string-upcase uri)))
    (symbol (split-at #\/ (symbol-name uri)))))

(defun get-param (request arg annotation)
  (aif (cdr (assoc arg (parameters request)))
       (handler-case
	   (funcall annotation (quri:url-decode it))
	 (error (c)
	   (declare (ignore c))
	   (error (make-instance 'http-assertion-error :assertion arg))))
       (error (make-instance 'http-assertion-error :assertion arg))))

(defun dedupe-params (full-params)
  (let ((dupes (make-hash-table))
	(params (make-hash-table)))
    (loop for (p annotation) in full-params
	  do (cond
	       ((and annotation
		     (gethash p params)
		     (not (eql annotation (gethash p params))))
		(incf (gethash p dupes 0)))
	       ((not (gethash p params))
		(setf (gethash p params) annotation))))
    (assert (every (lambda (n) (= n 0))
		   (alexandria:hash-table-values dupes))
	    nil "Found dupe in parameters: ~s"
	    (loop for k being the hash-keys of dupes
		  for v being the hash-values of dupes
		  when (/= v 0) collect k))
    (loop for p being the hash-keys of params
	  for ann being the hash-values of params
	  collect (list p ann))))

;;;;; Common HTTP types
(defun >>string (arg) arg)

(defun >>integer (arg)
  (etypecase arg
    (integer arg)
    (number (round arg))
    (string (let ((parsed (parse-integer arg :junk-allowed t)))
	      (assert (numberp parsed))
	      parsed))))

(defun >>json (arg)
  (json:decode-json-from-string arg))

(defun >>keyword (arg)
  (etypecase arg
    (keyword arg)
    (symbol (intern (symbol-name arg) :keyword))
    (string (intern (string-upcase arg) :keyword))))

(defun >>list (elem-type)
  (lambda (arg)
    (loop for elem in (json:decode-json-from-string arg)
	  collect (funcall elem-type elem))))

;;;;;;;;;; Defining Handlers
(defmacro make-closing-handler ((&key (content-type "text/html")) (&rest args) &body body)
  (with-gensyms (cookie?)
    `(lambda (sock ,cookie? session request)
       (declare (ignorable session request))
       (let* (,@(loop for a in args
		      collect `(,(car a) (get-arg request ,(car a) ,(cadr a))))
	      (headers (list (cons "Cache-Control" "no-cache, no-store, must-revalidate")
			     (cons "Access-Control-Allow-Origin" "*")
			     (cons "Access-Control-Allow-Headers" "Content-Type")))
	      (result (progn ,@body))
	      (response
		(if (typep result 'response)
		    result
		    (make-instance
		     'response
		     :content-type ,content-type
		     :cookie (unless ,cookie? (token session))
		     :body result))))
	 (setf (headers response) headers)
	 (write! response (flex-stream sock))
	 (socket-close sock)))))

(defmacro make-stream-handler ((&rest args) &body body)
  (with-gensyms (cookie?)
    `(lambda (sock ,cookie? session request)
       (declare (ignorable session request))
       ,(arguments args
		   `(let ((headers (list (cons "Cache-Control" "no-cache, no-store, must-revalidate")
					 (cons "Access-Control-Allow-Origin" "*")
					 (cons "Access-Control-Allow-Headers" "Content-Type")))
			  (res (progn ,@body))
			  (stream (flex-stream sock)))
		      (write!
		       (make-instance
			'response
			:keep-alive? t :content-type "text/event-stream"
			:headers headers
			:cookie (unless ,cookie? (token session)))
		       stream)
		      (crlf stream)
		      (write!
		       (make-instance 'sse :data (or res "Listening..."))
		       stream)
		      (force-output stream))))))

(defmacro define-handler ((name &key (close-socket? t) (content-type "text/html") (method :any)) (&rest args) &body body)
  (let* ((processed (process-uri name))
	 (path-vars (loop for v in processed when (path-var? v)
			  collect (list (intern (symbol-name (var-key v))) (var-annotation v))))
	 (full-params (dedupe-params (append args path-vars))))
    `(insert-handler!
      (list ,@(cons method processed))
      ,(if close-socket?
	   `(make-closing-handler (:content-type ,content-type) ,full-params ,@body)
	   `(make-stream-handler ,full-params ,@body)))))

(defmacro define-json-handler ((name &key (method :any)) (&rest args) &body body)
  `(define-handler (,name :content-type "application/json") ,args
     (json:encode-json-to-string (progn ,@body))))

;;;;; Special case handlers
;;; Don't use these in production. There are better ways.
(defmethod define-file-handler ((path pathname) &key stem-from (method :any))
  (cond ((cl-fad:directory-exists-p path)
	 (cl-fad:walk-directory
	  path
	  (lambda (fname)
	    (define-file-handler fname :stem-from (or stem-from (format nil "~a" path)) :method method))))
	((cl-fad:file-exists-p path)
	 (insert-handler!
	  (cons method (process-uri (path->uri path :stem-from stem-from)))
	  (let ((mime (path->mimetype path)))
	    (lambda (sock cookie? session request)
	      (declare (ignore cookie? session request))
	      (if (cl-fad:file-exists-p path)
		  (with-open-file (s path :direction :input :element-type 'octet)
		    (let ((buf (make-array (file-length s) :element-type 'octet)))
		      (read-sequence buf s)
		      (write!
		       (make-instance 'response :content-type mime :body buf)
		       (flex-stream sock)))
		    (socket-close sock))
		  (error! +404+ sock))))))
	(t
	 (warn "Tried serving nonexistent file '~a'" path)))
  nil)

(defmethod define-file-handler ((path string) &key stem-from)
  (define-file-handler (pathname path) :stem-from stem-from))

(defun redirect! (target &key permanent?)
  (make-instance
   'response
   :response-code (if permanent? "301 Moved Permanently" "307 Temporary Redirect")
   :location target :content-type "text/plain" :body "Resource moved..."))

(defmacro define-redirect-handler ((name &key permanent? (method :any)) target)
  (with-gensyms (cookie?)
    `(insert-handler!
      (list ,@(cons method (process-uri name)))
      (lambda (sock ,cookie? session request)
	(declare (ignorable sock ,cookie? session request))
	(write!
	 (redirect! ,target :permanent? ,permanent?)
	 (flex-stream sock))
	(socket-close sock)))))
