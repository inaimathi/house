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
  (is (-param-bindings '((a (>>LIST #'>>KEYWORD))))
      '((a (get-param request :a (>>list #'>>keyword))))
      "Leaves list annotations as-is")
  (is (-param-bindings '((a (>>LIST >>KEYWORD))))
      '((a (get-param request :a (>>list >>keyword))))
      "Leaves list annotations as-is even if that's not the right thing to do?f")
  (is (-param-bindings '((a #'>>keyword)))
      '((a (get-param request :a #'>>keyword)))
      "Leaves function-namespace annotations as-is")
  (is (-param-bindings '((a >>keyword)))
      '((a (get-param request :a (fdefinition '>>keyword))))
      "Converts variable-namespace annotation symbols to functions"))

 (subtest
  "closing-handler"
  (let ((res (funcall
	      (closing-handler
		  (:content-type "text/html") ((name >>keyword))
		(format nil "Hello, ~a!" name))
	      (make-instance
	       'request :http-method :GET :resource "/foo"
			:parameters '((:name . "inaimathi"))))))
    (is "200 OK" (response-code res))
    (is "Hello, INAIMATHI!" (body res))
    (is "text/html" (content-type res)))

  (let ((pr (redirect! "/foo" :permanent? t))
	(perm (funcall
	       (closing-handler
		   (:content-type "text/html") ((name >>keyword))
		 (redirect! "/foo" :permanent? t))
	       (make-instance
		'request :http-method :GET :resource "/foo"
			 :parameters '((:name . "inaimathi")))))
	(tm (redirect! "/foo"))
	(temp (funcall
	       (closing-handler
		   (:content-type "text/html") ((name >>keyword))
		 (redirect! "/foo"))
	       (make-instance
		'request :http-method :GET :resource "/foo"
			 :parameters '((:name . "inaimathi"))))))
    (is (body perm) (body pr))
    (is (body temp) (body tm))
    (is "text/plain" (content-type perm))
    (is "text/plain" (content-type temp))
    (is (response-code perm) (response-code pr))
    (is (response-code temp) (response-code tm)))

  (let ((res (funcall
	      (closing-handler
		  (:content-type "text/html") ((name >>keyword))
		(push (cons "X-Saying-Hello" "True") headers)
		(format nil "Hello, ~a!" name))
	      (make-instance
	       'request :http-method :GET :resource "/foo"
			:parameters '((:name . "inaimathi"))))))
    (is "200 OK" (response-code res))
    (is '("X-Saying-Hello" . "True")
	(assoc "X-Saying-Hello" (headers res) :test #'string=))
    (is "Hello, INAIMATHI!" (body res))))

 (subtest
  "stream-handler"
  (let ((res (funcall
	      (stream-handler (public-channel/name) ((name >>keyword))
		t)
	      (make-instance
	       'request :http-method :GET :resource "/foo"
			:parameters '((:name . "inaimathi"))
			:socket-of :subscribed-sock))))
    (is "text/event-stream" (content-type res)
	"The default Content-Type is text/event-stream")
    (is nil (body res)
	"The default body is empty")
    (is :subscribed-sock
	(car (member :subscribed-sock
		     (subscribers-of 'public-channel/name)))
	"When the channel handler returns a non-nil value, subscribe the given socket to the current channel"))

  (let ((res (funcall
  	      (stream-handler (private-channel/name) ((name >>keyword))
  		nil)
  	      (make-instance
  	       'request :http-method :GET :resource "/foo"
  			:parameters '((:name . "inaimathi"))
  			:socket-of :unsubscribed-sock))))
    (is nil
	(member :unsubscribed-sock
		(subscribers-of 'private-channel/name))
	"When the channel handler returns a NIL, the socket is not subscribed")))

 (subtest
  "define-handler and define-channel"
  (let ((tbl (define-handler
		 (foo/bar :content-type "text/html") ((name >>keyword))
	       (format nil "Hello, ~a!" name))))
    (is 'house::handler-entry (type-of (find-handler :GET "/foo/bar" :handler-table tbl))
	"define-handler inserts a handler-entry at the appropriate place in the handler table")
    (is t (closing? (find-handler :GET "/foo/bar" :handler-table tbl))
	"define-handler sets the handler-entry:closing? to T")
    (is "Hello, INAIMATHI!"
	(body
	 (funcall
	  (house::fn (find-handler :GET "/foo/bar" :handler-table tbl))
	  (make-instance
	   'request :http-method :GET :resource "/foo/bar"
		    :parameters '((:name . "inaimathi")))))
	"Calling the fn behaves as a call to the appropriate closing-handler would"))

  (let ((tbl (define-channel
		 (foo/bar/baz) ((name >>keyword))
	       (format nil "Hello, ~a!" name))))
    (is 'house::handler-entry (type-of (find-handler :GET "/foo/bar/baz" :handler-table tbl))
	"define-handler inserts a handler-entry at the appropriate place in the handler table")
    (is nil (closing? (find-handler :GET "/foo/bar/baz" :handler-table tbl))
	"define-handler sets the handler-entry:closing? to T")
    (is :subscribing-socket
	(progn (funcall
		(house::fn (find-handler :GET "/foo/bar/baz" :handler-table tbl))
		(make-instance
		 'request :http-method :GET :resource "/foo/bar"
		 :parameters '((:name . "inaimathi"))
		 :socket-of :subscribing-socket))
	       (car (member :subscribing-socket
			    (subscribers-of 'foo/bar/baz))))
	"Calling the fn behaves as a call to the appropriate stream-handler would"))))
