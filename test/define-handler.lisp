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
      '((A >>KEYWORD))))

 (subtest
  "-param-bindings"
  ;; TODO - expands symbol annotations to fdefinition calls
  ;; TODO - leaves list annotations as-is
  )

 (subtest
  "closing-handler"
  ;; TODO - if body returns a response (through redirect!), returin it
  ;; TODO - if body returns a string, make a response out of it
  ;; TODO - generated response has appropriate content-type
  ;; TODO - generated response contains appropriate body
  ;; TODO - the user can add `headers` from the body
  ;; TODO - the user can remove `headers` from the body
  )

 (subtest
  "stream-handler"
  ;; TODO - content-type is always `text/event-stream`
  ;; TODO - body is always empty
  ;; TODO - user can modify headers as above
  )

 (subtest
  "define-handler and define-channel"
  ;; TODO - expand into handler-entry instances
  ;; TODO - the closing? flag is set as appropriate
  ;; TODO - they insert the handler at the expected place in the current table
  ))
