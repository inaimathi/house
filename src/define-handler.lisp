(in-package #:house)

;;;;;;;;;; Parameter type parsing.
(defun get-param (request arg annotation)
  (let* ((raw (cdr (assoc arg (parameters request))))
	 (decoded (if (stringp raw) (quri:url-decode raw) raw)))
    (cond ((and decoded (not annotation)) decoded)
	  (annotation
	   (handler-case
	       (funcall annotation decoded)
	     (error (c)
	       (declare (ignore c))
	       (error (make-instance 'http-assertion-error :assertion arg)))))
	  (t (error (make-instance 'http-assertion-error :assertion arg))))))

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
(defun >>string (arg)
  (assert (stringp arg))
  arg)

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
  (assert arg)
  (etypecase arg
    (keyword arg)
    (symbol (intern (symbol-name arg) :keyword))
    (string (intern (string-upcase arg) :keyword))))

(defun >>list (elem-type)
  (lambda (arg)
    (loop for elem in (if (stringp arg) (json:decode-json-from-string arg) arg)
	  collect (funcall elem-type elem))))

;;;;;;;;;; Defining Handlers
(defstruct handler-entry
  (fn nil)
  (closing? t))

(defun -param-bindings (params)
  (loop for p in (dedupe-params params)
	collect (let ((f (if (and (cadr p) (symbolp (cadr p)))
			     `(fdefinition ',(cadr p))
			     (cadr p))))
		  `(,(car p) (get-param request ,(->keyword (car p)) ,f)))))

(defmacro closing-handler ((&key (content-type "text/html") headers) (&rest args) &body body)
  `(lambda (request)
     (declare (ignorable request))
     (let* (,@(-param-bindings args)
	    (headers (concatenate
		      'list ,headers
		      (list (cons "Cache-Control" "no-cache, no-store, must-revalidate")
			    (cons "Access-Control-Allow-Origin" "*")
			    (cons "Access-Control-Allow-Headers" "Content-Type"))))
	    (result (progn ,@body)))
       (if (typep result 'response)
	   result
	   (make-instance
	    'response
	    :content-type ,content-type
	    :headers headers
	    :body result)))))

(defmacro stream-handler ((channel-name &key (headers nil)) (&rest args) &body body)
  `(lambda (request)
     (declare (ignorable request))
     (let* (,@(-param-bindings args)
	    (headers
	      (concatenate
	       'list ,headers
	       (list (cons "Cache-Control" "no-cache, no-store, must-revalidate")
		     (cons "Access-Control-Allow-Origin" "*")
		     (cons "Access-Control-Allow-Headers" "Content-Type"))))
	    (result (progn ,@body)))
       (cond
	 ((typep result 'response) result)
	 (result
	  (aif (socket-of request)
	       (subscribe! ',channel-name it))
	  (make-instance
	   'response
	   :keep-alive? t :content-type "text/event-stream"
	   :headers headers))
	 (t +404+)))))

(defun -full-params (name args)
  (let* ((processed (process-uri name))
	 (path-vars (loop for v in processed when (path-var? v)
			  collect (list
				   (intern (symbol-name (var-key v)))
				   (var-annotation v)))))
    (append args path-vars)))

(defmacro define-channel ((name &key (method :any)) (&rest args) &body body)
  `(insert-handler!
    ,method ',name
    (make-handler-entry
     :fn (stream-handler (,name) ,(-full-params name args) ,@body)
     :closing? nil)))

(defmacro define-handler ((name &key (content-type "text/html") (method :any)) (&rest args) &body body)
  `(insert-handler!
    ,method ,(if (eq name 'root) "" `',name)
    (make-handler-entry
     :fn (closing-handler
	     (:content-type ,content-type)
	     ,(-full-params name args) ,@body)
     :closing? t)))

(defmacro define-json-handler ((name &key (method :any)) (&rest args) &body body)
  `(define-handler (,name :content-type "application/json") ,args
     (json:encode-json-to-string (progn ,@body))))

;;;;; Special case handlers
(defun define-file-handler (path &key stem-from (method :any))
  (warn "define-file-handler is not intended for production use")
  (let ((path (if (stringp path) (pathname path) path)))
    (cond ((cl-fad:directory-exists-p path)
	   (cl-fad:walk-directory
	    path
	    (lambda (fname)
	      (define-file-handler fname :stem-from (or stem-from (format nil "~a" path)) :method method))))
	  ((cl-fad:file-exists-p path)
	   (insert-handler!
	    method (path->uri path :stem-from stem-from)
	    (make-handler-entry
	     :fn (let ((mime (path->mimetype path)))
		   (lambda (request)
		     (declare (ignore request))
		     (if (cl-fad:file-exists-p path)
			 (with-open-file (s path :direction :input :element-type 'octet)
			   (let ((buf (make-array (file-length s) :element-type 'octet)))
			     (read-sequence buf s)
			     (make-instance 'response :content-type mime :body buf)))
			 +404+)))
	     :closing? t)))
	  (t
	   (warn "Tried serving nonexistent file '~a'" path))))
  nil)
