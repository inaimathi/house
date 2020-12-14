(in-package #:house/test)

(tests

 (subtest
  "parse-elems"
  (is '((:a . "1")) (parse-params :multipart "a=1"))
  (is '((:a . "1") (:b . "2")) (parse-params :multipart "a=1&b=2"))
  (is '((:longer . "parameter names") (:look . "something like this"))
      (parse-params :multipart "longer=parameter names&look=something like this"))
  (is '((:longer . "parameter%20names") (:look . "something%20like%20this"))
      (parse-params :multipart "longer=parameter%20names&look=something%20like%20this")))

 (subtest
  "Request parsing"
  (subtest
   "Older HTTP versions"
   (is-error
    (parse "GET /index.html HTTP/0.9
Host: www.example.com

")
    'http-assertion-error)
   (is-error
    (parse "GET /index.html HTTP/1.0
Host: www.example.com

")
    'http-assertion-error))

  (subtest
   "Vanilla GET"
   (let ((req (parse "GET /index.html HTTP/1.1
Host: www.example.com

")))
     (is "/index.html" (resource req))
     (is '((:host . "www.example.com")) (headers req))))

  (subtest
   "GET with params"
   (let ((req (parse "GET /index.html?test=1 HTTP/1.1
Host: www.example.com

")))
     (is "/index.html" (resource req))
     (is '((:host . "www.example.com")) (headers req))
     (is '(:test . "1") (assoc :test (parameters req)))))

  (subtest
   "POST with body"
   (let ((req (parse "POST /index.html HTTP/1.1
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
   (let ((req (parse "POST /index.html?get-test=get HTTP/1.1
Host: www.example.com
Content-length: 14

post-test=post
")))
     (is "/index.html" (resource req))
     (is '((:host . "www.example.com")) (headers req))
     (is '(:get-test . "get") (assoc :get-test (parameters req)))
     ;; (is '(:post-test . "post") (assoc :post-test (parameters req)))
     ))

  (subtest
   "Running server tests"

   ;; (defmethod read-all ((stream stream))
   ;;   (coerce
   ;;    (loop for char = (read-char-no-hang stream nil :eof)
   ;; 	    until (or (null char) (eq char :eof)) collect char into msg
   ;; 	    finally (return (values msg char)))
   ;;    'string))

   ;; (defmethod write! ((strings list) (stream stream))
   ;;   (mapc (lambda (seq)
   ;; 	     (write-sequence seq stream)
   ;; 	     (crlf stream))
   ;; 	   strings)
   ;;   (crlf stream)
   ;;   (force-output stream)
   ;;   (values))

   ;; (let* ((port 4321)
   ;; 	  (server (bt:make-thread (lambda () (start port)))))
   ;;   (sleep 1)
   ;;   (define-closing-handler (test :content-type "text/plain") ()
   ;;     "Hello!")
   ;;   (define-closing-handler (arg-test :content-type "text/plain") ((num :integer) (key :keyword) (num-list :list-of-integer))
   ;;     (format nil "~{~s~^ ~}" (list num key num-list)))
   ;;   (define-closing-handler (arg-test-two :content-type "text/plain") ((a :string) b (key-list :list-of-keyword) (json :json))
   ;;     (format nil "~{~s~^ ~}" (list a b key-list json)))
   ;;   (unwind-protect
   ;; 	  (labels ((parse-res (res)
   ;; 		     (destructuring-bind (hdr bdy) (cl-ppcre:split "\\r\\n\\r\\n" res)
   ;; 		       (list (cl-ppcre:split "\\r\\n" hdr)
   ;; 			     (cl-ppcre:regex-replace "\\r\\n" bdy ""))))
   ;; 		   (req (&rest lines)
   ;; 		     (with-client-socket (sock stream "localhost" port)
   ;; 		       (write! lines stream)
   ;; 		       (when (wait-for-input sock :timeout 2 :ready-only t)
   ;; 			 (parse-res (read-all stream))))))
   ;; 	    (destructuring-bind (headers body) (req "GET /test HTTP/1.1")
   ;; 	      (is "HTTP/1.1 200 OK" (first headers))
   ;; 	      (is "Hello!" body))
   ;; 	    (destructuring-bind (headers body) (req "POST /test HTTP/1.1")
   ;; 	      (is "HTTP/1.1 200 OK" (first headers))
   ;; 	      (is "Hello!" body))
   ;; 	    (destructuring-bind (headers body) (req "POST /arg-test HTTP/1.1" "" "num=1&key=test&num-list=%5B1%2C2%2C3%2C4%2C5%5D")
   ;; 	      (is "HTTP/1.1 200 OK" (first headers))
   ;; 	      (is "1 :TEST (1 2 3 4 5)" body))
   ;; 	    (destructuring-bind (headers body) (req "POST /arg-test-two HTTP/1.1")
   ;; 	      (is "HTTP/1.1 400 Bad Request" (first headers))
   ;; 	      (is "Malformed, or slow HTTP request..." body))
   ;; 	    (destructuring-bind (headers body) (req "POST /arg-test-two HTTP/1.1" "" "a=test&b=blah&key-list=%5B%22one%22%2C%22two%22%2C%22three%22%5D&json=%5B%22one%22%2C%22two%22%2C%22three%22%5D")
   ;; 	      (is "HTTP/1.1 200 OK" (first headers))
   ;; 	      (is "\"test\" \"blah\" (:ONE :TWO :THREE) (\"one\" \"two\" \"three\")" body)))
   ;;     (ignore-errors
   ;; 	(bt:destroy-thread server))))
   )))
