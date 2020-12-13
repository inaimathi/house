(in-package #:house)

;;;;;;;;;; Parameter type parsing.
;;;;; Basics
(defparameter *http-type-priority* (make-hash-table)
  "Priority table for all parameter types.
Types will be parsed from highest to lowest priority;
parameters with a lower priority can refer to parameters of a higher priority.")

(defgeneric type-expression (parameter type)
  (:documentation
   "A type-expression will tell the server how to convert a parameter from a string to a particular, necessary type."))
(defgeneric type-assertion (parameter type)
  (:documentation
   "A lookup assertion is run on a parameter immediately after conversion. Use it to restrict the space of a particular parameter."))
(defmethod type-expression (parameter type) nil)
(defmethod type-assertion (parameter type) nil)

;;;;; Definition macro
(defmacro define-http-type ((type &key (priority 0)) &key type-expression type-assertion)
  (assert (numberp priority) nil "`priority` should be a number. The highest will be converted first")
  (with-gensyms (tp)
    `(let ((,tp ,type))
       (setf (gethash ,tp *http-type-priority*) ,priority)
       ,@(when type-expression
	   `((defmethod type-expression (parameter (type (eql ,type))) ,type-expression)))
       ,@(when type-assertion
	   `((defmethod type-assertion (parameter (type (eql ,type))) ,type-assertion))))))

;;;;; Common HTTP types
(define-http-type (:string))

(define-http-type (:integer)
    :type-expression `(parse-integer ,parameter :junk-allowed t)
    :type-assertion `(numberp ,parameter))

(define-http-type (:json)
    :type-expression `(json:decode-json-from-string ,parameter))

(define-http-type (:keyword)
    :type-expression `(->keyword ,parameter))

(define-http-type (:list-of-keyword)
    :type-expression `(loop for elem in (json:decode-json-from-string ,parameter)
			 if (stringp elem) collect (->keyword elem)
			 else do (error (make-instance 'http-assertion-error :assertion `(stringp ,elem)))))

(define-http-type (:list-of-integer)
    :type-expression `(json:decode-json-from-string ,parameter)
    :type-assertion `(every #'numberp ,parameter))

;;;;;;;;;; Constructing argument lookups
(defun args-by-type-priority (args &optional (priority-table *http-type-priority*))
  (let ((cpy (copy-list args)))
    (sort cpy #'<=
	  :key (lambda (arg)
		 (if (listp arg)
		     (gethash (second arg) priority-table 0)
		     0)))))

(defun arg-exp (arg-sym)
  `(aif (cdr (assoc ,(->keyword arg-sym) (parameters request)))
	(uri-decode it)
	(error (make-instance 'http-assertion-error :assertion ',arg-sym))))

(defun arguments (args body)
  (loop with res = body
     for arg in (args-by-type-priority args)
     do (match arg
	  ((guard arg-sym (symbolp arg-sym))
	   (setf res `(let ((,arg-sym ,(arg-exp arg-sym)))
			,res)))
	  ((list* arg-sym type restrictions)
	   (setf res
		 `(let ((,arg-sym ,(or (type-expression (arg-exp arg-sym) type) (arg-exp arg-sym))))
		    ,@(awhen (type-assertion arg-sym type) `((assert-http ,it)))
		    ,@(loop for r in restrictions collect `(assert-http ,r))
		    ,res))))
     finally (return res)))

;;;;;;;;;; Defining Handlers
(defmacro make-closing-handler ((&key (content-type "text/html")) (&rest args) &body body)
  (with-gensyms (cookie?)
    `(lambda (sock ,cookie? session request)
       (declare (ignorable session request))
       ,(arguments args
		   `(let* ((headers (list (cons "Cache-Control" "no-cache, no-store, must-revalidate")
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
		      (socket-close sock))))))

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

(defun parse-var (str)
  (let ((pair (split-at #\= (string-upcase (subseq str 1)))))
    (if (second pair)
	(list (intern (first pair)) (intern (second pair) :keyword))
	(intern (first pair)))))

(defun check-for-dupes (full-params)
  (let ((dupes (make-hash-table)))
    (loop for arg in full-params
       for a = (if (consp arg) (car arg) arg)
       do (incf (gethash a dupes 0)))
    (assert (every (lambda (n) (= n 1)) (alexandria:hash-table-values dupes))
	    nil "Found dupe in parameters: ~s"
	    (loop for k being the hash-keys of dupes for v being the hash-values of dupes
	       when (/= v 1) collect k))))

(defmacro define-handler ((name &key (close-socket? t) (content-type "text/html") (method :any)) (&rest args) &body body)
  (let* ((processed (process-uri name))
	 (path-vars (loop for v in processed when (path-var? v) collect (parse-var v)))
	 (full-params (append args path-vars)))
    (check-for-dupes full-params)
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
