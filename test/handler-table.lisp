(in-package #:house/test)

(tests
 (subtest
  "URI Variable destructuring"
  (is (var-key "foo") nil)
  (is (var-annotation "foo") nil)
  (is (var-key "<a>") :a)
  (is (var-annotation "<a>") nil)
  (is (var-key "<foo>") :foo)
  (is (var-annotation "<foo>") nil)
  (is (var-key "<foo>=>>string") :foo)
  (is (var-annotation "<foo>=>>string") '>>string)))

;; (tests
;;  (subtest
;;   "Argument routing"
;;   (let ((tbl (with-handler-table (empty)
;; 	       (define-closing-handler (test :content-type "text/plain") ()
;; 		 "Hello!")
;; 	       (define-closing-handler (arg-test :content-type "text/plain") ((num :integer) (key :keyword) (num-list :list-of-integer))
;; 		 (format nil "~{~s~^ ~}" (list num key num-list)))
;; 	       (define-closing-handler (arg-test-two :content-type "text/plain") ((a :string) b (key-list :list-of-keyword) (json :json))
;; 		 (format nil "~{~s~^ ~}" (list a b key-list json))))))
;;     (is (find-handler :get "/test")))
;;   (let* ((port 4321)
;;    	 (server (bt:make-thread (lambda () (start port)))))
;;     (sleep 1)


;;     (unwind-protect
;;    	 (labels ((parse-res (res)
;;    		    (destructuring-bind (hdr bdy) (cl-ppcre:split "\\r\\n\\r\\n" res)
;;    		      (list (cl-ppcre:split "\\r\\n" hdr)
;;    			    (cl-ppcre:regex-replace "\\r\\n" bdy ""))))
;;    		  (req (&rest lines)
;;    		    (with-client-socket (sock stream "localhost" port)
;;    		      (write! lines stream)
;;    		      (when (wait-for-input sock :timeout 2 :ready-only t)
;;    			(parse-res (read-all stream))))))
;;    	   (destructuring-bind (headers body) (req "GET /test HTTP/1.1")
;;    	     (is "HTTP/1.1 200 OK" (first headers))
;;    	     (is "Hello!" body))
;;    	   (destructuring-bind (headers body) (req "POST /test HTTP/1.1")
;;    	     (is "HTTP/1.1 200 OK" (first headers))
;;    	     (is "Hello!" body))
;;    	   (destructuring-bind (headers body) (req "POST /arg-test HTTP/1.1" "" "num=1&key=test&num-list=%5B1%2C2%2C3%2C4%2C5%5D")
;;    	     (is "HTTP/1.1 200 OK" (first headers))
;;    	     (is "1 :TEST (1 2 3 4 5)" body))
;;    	   (destructuring-bind (headers body) (req "POST /arg-test-two HTTP/1.1")
;;    	     (is "HTTP/1.1 400 Bad Request" (first headers))
;;    	     (is "Malformed, or slow HTTP request..." body))
;;    	   (destructuring-bind (headers body) (req "POST /arg-test-two HTTP/1.1" "" "a=test&b=blah&key-list=%5B%22one%22%2C%22two%22%2C%22three%22%5D&json=%5B%22one%22%2C%22two%22%2C%22three%22%5D")
;;    	     (is "HTTP/1.1 200 OK" (first headers))
;;    	     (is "\"test\" \"blah\" (:ONE :TWO :THREE) (\"one\" \"two\" \"three\")" body)))
;;       (ignore-errors
;;        (bt:destroy-thread server))))
;;   ))
