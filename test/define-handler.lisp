(in-package #:house/test)

(tests
 (subtest
  "Request get-param"
  (let ((req (make-instance
	      'request :parameters
	      `((:a . "foo") (:b . "bar") (:c . "1337")
		(:d . ,(quri:url-encode "A longer & \"fully-encoded\" string"))
		(:e . "{\"a\": 1, \"b\": 2}")
		(:f . "[1, 2, 3]") (:g . "[\"foo\", \"bar\"]")))))
    (is (get-param req :a #'>>string) "foo")
    (is (get-param req :b #'>>keyword) :bar)
    (is (get-param req :c #'>>integer) 1337)
    (is (get-param req :d #'>>string)
	"A longer & \"fully-encoded\" string")
    (is (get-param req :e #'>>json)
	'((:a . 1) (:b . 2)))
    (is (get-param req :f (>>list #'>>integer))
	(list 1 2 3))
    (is (get-param req :g (>>list #'>>keyword))
	(list :foo :bar))
    (is-error (get-param req :nonexistant-arg #'>>string) 'http-assertion-error)
    (is-error (get-param req :a #'>>integer ) 'http-assertion-error)))

 (subtest
  "dedupe-params"
  (is-error (dedupe-params '((A >>INTEGER) (A >>KEYWORD))) 'error)
  (is-error (dedupe-params
	     '((A >>INTEGER) (A >>KEYWORD)
	       (B >>INTEGER) (B >>KEYWORD)))
	    'error)
  (is (dedupe-params '((A >>KEYWORD) (B (>>LIST >>INTEGER))))
      '((A >>KEYWORD) (B (>>LIST >>INTEGER))))
  (is (dedupe-params '((A >>KEYWORD) (A >>KEYWORD)))
      '((A >>KEYWORD)))
  (is (dedupe-params '((A >>KEYWORD) (A)))
      '((A >>KEYWORD)))))
