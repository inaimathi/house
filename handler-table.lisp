(in-package :cl-handlers)

;;;;; A minimal, custom Trie
;;;;;;;; (It needs to allow for variables at each level, including prospective matching of the rest of a URI segment)
(defstruct trie
  (value nil)
  (map (make-hash-table :test 'equal))
  (vars (make-hash-table)))

(defun any-vars? (trie)
  (> (hash-table-count (trie-vars trie)) 0))

(defun path-var? (str) (and (stringp str) (eql #\- (char str 0))))

(defun var-key (str)
  (let ((pair (split-at #\= (string-upcase (subseq str 1)))))
    (intern (car pair) :keyword)))

(defun trie-insert! (key value trie)
  (labels ((rec (key-parts trie)
             (cond ((null key-parts)
                    (setf (trie-value trie) value))
                   ((path-var? (first key-parts))
                    (next! (var-key (first key-parts)) (rest key-parts) (trie-vars trie)))
                   (t
                    (next! (first key-parts) (rest key-parts) (trie-map trie)))))
           (next! (k rest map)
             (let ((next (gethash k map)))
               (if next
                   (rec rest next)
                   (rec rest (setf (gethash k map) (make-trie)))))))
    (rec key trie)
    trie))

(defun trie-lookup (key trie)
  (labels ((rec (key-parts trie bindings)
             (if key-parts
                 (let ((next (gethash (canonical (first key-parts)) (trie-map trie))))
                   (cond (next
                          (rec (rest key-parts) next bindings))
                         ((any-vars? trie)
                          (loop for k being the hash-keys of (trie-vars trie)
                             for v being the hash-values of (trie-vars trie)
                             do (multiple-value-bind (val bindings)
                                    (rec (rest key-parts) v (cons (cons k (first key-parts)) bindings))
                                  (when val
                                    (return-from trie-lookup (values val bindings))))))
                         (t
                          nil)))
                 (values (trie-value trie) bindings)))
	   (canonical (thing)
	     (typecase thing
	       (string (string-upcase thing))
	       (t thing))))
    (rec key trie nil)))

;;;;; And using it to structure our handler table
(defclass handler-table ()
  ((handlers :initform (make-trie) :initarg :handlers :reader handlers)
   (error-handlers :initform (make-errors-table) :initarg :error-handlers :reader error-handlers)))

(defun make-errors-table ()
  (let ((tbl (make-hash-table)))
    (mapc
     (lambda (pair)
       (setf
	(gethash (first pair) tbl)
	`(,(first pair) (:content-type "text/plain") (,(second pair)))))
     '((400 "Request error")
       (404 "Not found")
       (500 "Something went wrong internally. Please make a note of it.")))
    tbl))

(defun empty () (make-instance 'handler-table))

(defparameter *handler-table* (empty))

(defun process-uri (uri)
  (split-at #\/ (symbol-name uri)))

(defun insert-handler! (uri handler-fn &key (handler-table *handler-table*))
  (trie-insert! uri handler-fn (handlers handler-table))
  handler-table)

(defun find-handler (method uri-string &key (handler-table *handler-table*))
  (trie-lookup
   (cons method (split-at #\/ uri-string))
   (handlers handler-table)))

(defun insert-error! (http-code error &key (handler-table *handler-table*))
  (setf (gethash http-code (error-handlers handler-table)) error))

(defun find-error (http-code &key (handler-table *handler-table*))
  (gethash http-code (error-handlers handler-table)))

(defmacro with-handler-table (tbl &body body)
  `(let ((*handler-table* ,tbl))
     ,@body))
