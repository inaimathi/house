(in-package #:house/test)

(tests
 (subtest
  "Request get-arg"
  (let ((req (make-instance
	      'request :parameters
	      `((:a . "foo") (:b . "bar") (:c . "1337")
		(:d . ,(quri:url-encode "A longer & \"fully-encoded\" string"))
		(:e . "{\"a\": 1, \"b\": 2}")
		(:f . "[1, 2, 3]") (:g . "[\"foo\", \"bar\"]")))))
    (is (get-arg req :a #'>>string) "foo")
    (is (get-arg req :b #'>>keyword) :bar)
    (is (get-arg req :c #'>>integer) 1337)
    (is (get-arg req :d #'>>string)
	"A longer & \"fully-encoded\" string")
    (is (get-arg req :e #'>>json)
	'((:a . 1) (:b . 2)))
    (is (get-arg req :f (>>list #'>>integer))
	(list 1 2 3))
    (is (get-arg req :g (>>list #'>>keyword))
	(list :foo :bar))
    (is-error (get-arg req :nonexistant-arg #'>>string) 'http-assertion-error)
    (is-error (get-arg req :a #'>>integer ) 'http-assertion-error)))

 (subtest
  "check-for-dupes"
  ;; correctly errors on collisions
  ;; correctly passes on no collisions
  ;; collapses non-conflicting duplicates
  ))
