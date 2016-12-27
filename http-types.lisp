(in-package #:house)

(defgeneric type-expression (parameter type)
  (:documentation
   "A type-expression will tell the server how to convert a parameter from a string to a particular, necessary type."))
(defgeneric type-assertion (parameter type)
  (:documentation
   "A lookup assertion is run on a parameter immediately after conversion. Use it to restrict the space of a particular parameter."))
(defmethod type-expression (parameter type) nil)
(defmethod type-assertion (parameter type) nil)

;;;;; Definition macro
(defun make-http-type-expander (type)
  (optima.extra:lambda-ematch
    ((list* (and method-name (or :type-expression :type-assertion)) (list parameter) method-body)
     `(defmethod ,(intern (symbol-name method-name) (find-package "HOUSE")) (,parameter (type (eql ,type)))
        ,@method-body))))

(defmacro define-http-type ((type &key (priority 0)) &body body)
  (declare (ignore priority))
  `(progn
     ,@(mapcar (make-http-type-expander type) body)))

;;;;; Common HTTP types
(define-http-type (:string))

(define-http-type (:integer)
  (:type-expression (parameter) (parse-integer parameter :junk-allowed t))
  (:type-assertion (parameter) (numberp parameter)))

(define-http-type (:integer)
  (:type-expression (parameter) (parse-integer parameter :junk-allowed t))
  (:type-assertion (parameter) (numberp parameter)))

(define-http-type (:json)
  (:type-expression (parameter) (json:decode-json-from-string parameter)))

(define-http-type (:keyword)
  (:type-expression (parameter) (->keyword parameter)))

(define-http-type (:list-of-keyword)
  (:type-expression (parameter) (loop for elem in (json:decode-json-from-string parameter)
                                      if (stringp elem) collect (->keyword elem)
                                        else do (error (make-instance 'http-assertion-error :assertion `(stringp ,elem))))))

(define-http-type (:list-of-integer)
  (:type-expression (parameter) (json:decode-json-from-string parameter))
  (:type-assertion (parameter) (every #'numberp parameter)))
