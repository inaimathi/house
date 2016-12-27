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
(defmacro define-http-type ((type &key (priority 0)) &key type-expression type-assertion)
  (declare (ignore priority))
  (assert (numberp priority) nil "`priority` should be a number. The highest will be converted first")
  (with-gensyms (tp)
    `(let ((,tp ,type))
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
