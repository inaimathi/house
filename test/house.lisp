(in-package #:house/test)

(tests

 (subtest
  "parse-param-string"
  (is '((:a . "1")) (parse-param-string "a=1"))
  (is '((:a . "1") (:b . "2")) (parse-param-string "a=1&b=2"))
  (is '((:longer . "parameter names") (:look . "something like this"))
      (parse-param-string "longer=parameter names&look=something like this"))
  (is '((:longer . "parameter%20names") (:look . "something%20like%20this"))
      (parse-param-string "longer=parameter%20names&look=something%20like%20this")))

 (subtest
  "Request parsing"
  (subtest
   "Older HTTP versions"
   (is-error
    (parse-request-string "GET /index.html HTTP/0.9
Host: www.example.com

")
    'http-assertion-error)
   (is-error
    (parse-request-string "GET /index.html HTTP/1.0
Host: www.example.com

")
    'http-assertion-error))

  (subtest
   "Vanilla GET"
   (let ((req (parse-request-string "GET /index.html HTTP/1.1
Host: www.example.com

")))
     (is "/index.html" (resource req))
     (is '((:host . "www.example.com")) (headers req))))

  (subtest
   "GET with params"
   (let ((req (parse-request-string "GET /index.html?test=1 HTTP/1.1
Host: www.example.com

")))
     (is "/index.html" (resource req))
     (is '((:host . "www.example.com")) (headers req))
     (is '(:test . "1") (assoc :test (parameters req)))))

  (subtest
   "POST with body"
   (let ((req (parse-request-string "POST /index.html HTTP/1.1
Host: www.example.com
Content-length: 6

test=1
")))
     (is "/index.html" (resource req))
     (is '((:host . "www.example.com")) (headers req))
     ;; (is '(:test . "1") (assoc :test (parameters req)))
     ))

  (subtest
   "POST with parameters and body"
   (let ((req (parse-request-string "POST /index.html?get-test=get HTTP/1.1
Host: www.example.com
Content-length: 14

post-test=post
")))
     (is "/index.html" (resource req))
     (is '((:host . "www.example.com")) (headers req))
     (is '(:get-test . "get") (assoc :get-test (parameters req)))
     ;; (is '(:post-test . "post") (assoc :post-test (parameters req)))
     )))

 (subtest
  "Response framing"
  (flet ((wire (res)
           (flex:with-output-to-sequence (out)
             (house::write-response! res (flex:make-flexi-stream out :external-format :utf-8))))
         (octets (str) (flex:string-to-octets str :external-format :utf-8)))
    (let* ((body "Jérôme ✓")
           (bytes (wire (make-instance 'house::response :body body)))
           (text (flex:octets-to-string bytes :external-format :utf-8)))
      (ok (search (format nil "Content-Length: ~a~c~c" (length (octets body)) #\Return #\Linefeed) text)
          "Content-Length counts UTF-8 bytes, not characters")
      (is (octets (format nil "~c~c~c~c~a" #\Return #\Linefeed #\Return #\Linefeed body))
          (subseq bytes (- (length bytes) (length (octets body)) 4))
          :test #'equalp
          "Headers end with a blank line and nothing follows the body")
      (ok (search "Connection: close" text) "Closing responses say so"))
    (let ((text (flex:octets-to-string
                 (wire (make-instance 'house::response :keep-alive? t :content-type "text/event-stream"))
                 :external-format :utf-8)))
      (ok (search "Connection: keep-alive" text) "Stream responses stay open")
      (ok (alexandria:ends-with-subseq (format nil "~c~c~c~c" #\Return #\Linefeed #\Return #\Linefeed) text)
          "Bodiless stream responses still terminate their headers"))))

 (subtest
  "Parameter decoding"
  (is "héllo there" (house::url-decode-param "h%C3%A9llo%20there"))
  (is "héllo" (house::url-decode-param "héllo") "Already-decoded non-ASCII passes through")
  (is 42 (house::url-decode-param 42)))

 (subtest
  "Session tokens"
  (let ((toks (loop repeat 1000 collect (house::new-session-token!))))
    (ok (every (lambda (tk) (= 64 (length tk))) toks) "1000 tokens generated without error")
    (ok (every (lambda (tk) (every #'alphanumericp tk)) toks))
    (is 1000 (length (remove-duplicates toks :test #'string=))))))
