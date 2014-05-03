(in-package :house)

(setf *print-failures* t
      *print-summary* t)

(define-test parameter-parsing 
  (assert-equal (parse-params "a=1") '((:a . "1")))
  (assert-equal (parse-params "a=1&b=2") '((:a . "1") (:b . "2")))
  (assert-equal (parse-params "longer=parameter names&look=something like this")
		'((:longer . "parameter names") (:look . "something like this")))
  ;; Nope, no automatic argument decoding (the default define-*-handler does decoding internally though)
  (assert-equal (parse-params "longer=parameter%20names&look=something%20like%20this")
		'((:longer . "parameter%20names") (:look . "something%20like%20this"))))

(define-test uri-decoding
  (assert-equal (uri-decode "test test") "test test")
  (assert-equal (uri-decode "test+test") "test test") ;; we expect encodeURIComponent on the content
  (assert-equal (uri-decode "test%20test") "test test")
  (assert-equal (uri-decode "%2C.%2F%3C%3E%3F%3A%22%3B'%5B%5D%7B%7D~!%40%23%24%25%5E%26*()_%2B-%3D%60%20")
		",./<>?:\";'[]{}~!@#$%^&*()_+-=` "))

(define-test request-parsing
  ;; Fail on older HTTP versions
  (assert-error 'http-assertion-error 
		(parse "GET /index.html HTTP/0.9
Host: www.example.com"))
  (assert-error 'http-assertion-error
		(parse "GET /index.html HTTP/1.0
Host: www.example.com"))

  ;; vanilla GET request
  (let ((req (parse "GET /index.html HTTP/1.1
Host: www.example.com"
)))
    (assert-equal (resource req) "/index.html")
    (assert-equal (headers req) '((:host . "www.example.com"))))

  ;; GET with parameters
  (let ((req (parse "GET /index.html?test=1 HTTP/1.1
Host: www.example.com"
)))
    (assert-equal (resource req) "/index.html")
    (assert-equal (headers req) '((:host . "www.example.com")))
    (assert-equal (assoc :test (parameters req)) '(:test . "1")))

  ;; POST with body
  (let ((req (parse "POST /index.html HTTP/1.1
Host: www.example.com

test=1
")))
    (assert-equal (resource req) "/index.html")
    (assert-equal (headers req) '((:host . "www.example.com")))
    (assert-equal (assoc :test (parameters req)) '(:test . "1")))

  ;; POST with parameters and body
  (let ((req (parse "POST /index.html?get-test=get HTTP/1.1
Host: www.example.com

post-test=post
")))
    (assert-equal (resource req) "/index.html")
    (assert-equal (headers req) '((:host . "www.example.com")))
    (assert-equal (assoc :get-test (parameters req)) '(:get-test . "get"))
    (assert-equal (assoc :post-test (parameters req)) '(:post-test . "post")))

  (let ((req (parse "POST /index.html?test=get HTTP/1.1
Host: www.example.com

test=post
")))
    (assert-equal (resource req) "/index.html")
    (assert-equal (headers req) '((:host . "www.example.com")))
    (assert-equal (parameters req) '((:test . "get") (:test . "post")))
    (assert-equal (assoc :test (parameters req)) '(:test . "get"))))
