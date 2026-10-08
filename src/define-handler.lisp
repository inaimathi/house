(in-package #:house)

;;;;;;;;;; Parameter type parsing.
(defun url-decode-param (raw)
  ;; Percent-encoded text is pure ASCII. QURI:URL-DECODE treats each char of
  ;; its input as a raw byte, so a string that's already decoded (a JSON
  ;; string value, a raw UTF-8 form field) makes it signal an encoding error.
  (if (and (stringp raw) (every (lambda (c) (< (char-code c) 128)) raw))
      (handler-case (quri:url-decode raw) (error () raw))
      raw))

(defun get-param (request arg annotation)
  (let* ((raw (cdr (assoc arg (parameters request))))
	 (decoded (url-decode-param raw)))
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

;;;;; Static directory mounts
(defun define-file-handler (root &key (prefix "/") (method :any))
  "Mount ROOT beneath a literal URL PREFIX. Files are looked up per request.
Use STATIC-FILE from a normal handler to serve individual or authorized files."
  (let* ((root (or (cl-fad:directory-exists-p root)
                   (error "Static root is not an existing directory: ~s" root)))
         (root (truename root))
         (prefix (string-right-trim "/" prefix)))
    (unless (or (string= prefix "")
                (and (char= #\/ (char prefix 0))
                     (every (lambda (c) (or (alphanumericp c) (find c "/-_"))) prefix)))
      (error "Static URL prefix must be an absolute literal path: ~s" prefix))
    (insert-handler!
     method prefix
     (make-handler-entry
      :fn (lambda (request)
            (let* ((resource (resource request))
                   (relative (subseq resource (min (length resource) (1+ (length prefix)))))
                   ;; URL paths preserve '+'. Decode once, strictly, before the
                   ;; filesystem policy runs. Malformed encodings yield 404.
                   (decoded (handler-case
                                (unless (cl-ppcre:scan "%(?![0-9a-fA-F]{2})" relative)
                                  (quri:url-decode (cl-ppcre:regex-replace-all "\\+" relative "%2B")
                                                   :encoding :utf-8))
                              (error () nil))))
              (if decoded (static-file decoded :root root) +404+)))
      :closing? t)
     :prefix t)))
