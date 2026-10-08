(in-package #:house/test)

(defun static-test-bytes ()
  (let ((bytes (make-array 40000 :element-type '(unsigned-byte 8))))
    (dotimes (i (length bytes) bytes) (setf (aref bytes i) (mod i 256)))))

(defun write-test-file (path contents)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
    (write-sequence (if (stringp contents) (utf8 contents) contents) out)))

(defun call-with-static-tree (fn)
  (let* ((base (merge-pathnames (format nil "house-static-~a-~a/" (get-universal-time)
                                      (random most-positive-fixnum))
                               (uiop:temporary-directory)))
         (root (merge-pathnames "public/" base)))
    (unwind-protect
         (progn
           (write-test-file (merge-pathnames "hello.txt" root) "héllo ✓")
           (write-test-file (merge-pathnames "sub/data.bin" root)
                            (static-test-bytes))
           (write-test-file (merge-pathnames "empty.txt" root) "")
           (write-test-file (merge-pathnames "a+b.txt" root) "plus")
           (write-test-file (merge-pathnames "literal%2e.txt" root) "percent")
           (write-test-file (merge-pathnames "secret.txt" base) "outside")
           (write-test-file (merge-pathnames "public-extra/secret.txt" base) "sibling")
           (funcall fn root base))
      ;; Unlink test symlinks before recursive cleanup (particularly parent ->
      ;; BASE), regardless of whether an assertion or setup operation failed.
      (dolist (name '("inside.txt" "outside.txt" "sibling" "parent" "broken"))
        (ignore-errors (delete-file (merge-pathnames name root))))
      (when (probe-file base) (uiop:delete-directory-tree base :validate t)))))

(defmacro with-static-tree ((root base) &body body)
  `(call-with-static-tree (lambda (,root ,base) ,@body)))

(defun resolve-test-file (root path)
  (house::resolve-static-file (static-file path :root root)))

(defun static-request (method target &optional header value)
  (let ((bytes (request-bytes method target)))
    (if header
        (concatenate '(vector (unsigned-byte 8))
                     (subseq bytes 0 (- (length bytes) 2))
                     (utf8 (format nil "~a: ~a~c~c~c~c" header value
                                   #\Return #\Linefeed #\Return #\Linefeed)))
        bytes)))

(defun file-response (port method target &optional header value)
  (call-with-client
   port (lambda (sock)
          (send-octets sock (static-request method target header value))
          (let ((deadline (wire-deadline)))
            ;; HEAD advertises the GET length but has no body.
            (let ((res (read-wire-response sock deadline :head (string= method "HEAD"))))
              (is (wire-byte sock deadline) :eof "Response ends immediately after its body")
              res)))))

(defun call-with-static-server (root fn)
  (call-with-local-server
   (lambda ()
     (define-file-handler root :prefix "/assets")
     (define-handler (download) () (static-file "hello.txt" :root root))
     (let ((descriptor (static-file "hello.txt" :root root)))
       (define-handler (reused) () descriptor))
     (define-handler (assets/special) () "explicit")
     (define-file-handler (merge-pathnames "sub/" root) :prefix "/assets/nested"))
   fn))

(tests
 (subtest
  "HTTP date parsing accepts all three formats and rejects invalid calendars"
  (let ((time (encode-universal-time 5 4 3 2 1 2020 0)))
    (dolist (text '("Thu, 02 Jan 2020 03:04:05 GMT" "Thursday, 02-Jan-20 03:04:05 GMT"
                    "Thu Jan  2 03:04:05 2020"))
      (is (house::parse-http-date text) time))
    (is (house::parse-http-date (house::http-date time)) time)
    (dolist (text '("invalid" "Mon, 31 Feb 2020 03:04:05 GMT"
                    "Thu, 02 XXX 2020 03:04:05 GMT" "Thu, 02 Jan 2020 25:04:05 GMT"))
      (is (house::parse-http-date text) nil))))

 (subtest
  "Relative names and canonical containment, including Windows path syntax"
  (with-static-tree (root base)
    (declare (ignore base))
    (is (resolve-test-file root "hello.txt") (truename (merge-pathnames "hello.txt" root)))
    (is (resolve-test-file root "sub/data.bin") (truename (merge-pathnames "sub/data.bin" root)))
    (is (resolve-test-file root "literal%2e.txt") (truename (merge-pathnames "literal%2e.txt" root))
        "Descriptor filenames are not URL-decoded")
    (dolist (path '("" "." ".." "../secret.txt" "sub/../../secret.txt" "/secret.txt"
                    "sub//data.bin" "sub/./data.bin" "sub/../hello.txt" "sub"
                    "*.txt" "hello?.txt" "C:/secret.txt" "C:secret.txt" "hello.txt:stream"
                    "\\secret.txt" "..\\secret.txt" "//server/share/file" "NUL" "con.txt" "CONOUT$"
                    "hello.txt." "hello.txt " "missing.txt"))
      (is (resolve-test-file root path) nil (format nil "Reject ~s" path)))
    (is (resolve-test-file root (format nil "hello.txt~c" (code-char 0))) nil)
    (is-error (static-file "hello.txt") 'error "A root must be explicit")))

 (subtest
  "Symlinks may stay inside the root but cannot escape to a sibling or parent"
  #+(or unix linux darwin)
  (with-static-tree (root base)
    (flet ((link (target name)
             (uiop:run-program (list "ln" "-s" (namestring target)
                                     (namestring (merge-pathnames name root))))))
      (link (merge-pathnames "hello.txt" root) "inside.txt")
      (link (merge-pathnames "secret.txt" base) "outside.txt")
      (link (merge-pathnames "public-extra/" base) "sibling")
      (link base "parent")
      (link (merge-pathnames "missing" base) "broken")
      (is (resolve-test-file root "inside.txt") (truename (merge-pathnames "hello.txt" root)))
      (dolist (name '("outside.txt" "sibling/secret.txt" "parent/secret.txt"))
        (is (resolve-test-file root name) nil (format nil "Reject symlink ~a" name)))
      (call-with-static-server root
       (lambda (port)
         (dolist (name '("outside.txt" "sibling/secret.txt" "parent/secret.txt" "broken"))
           (is (wire-response-status (file-response port "GET" (concatenate 'string "/assets/" name)))
               "HTTP/1.1 404 Not Found"))))))
  #-(or unix linux darwin) (skip 1 "Symlink creation test requires a Unix ln command"))

 (subtest
  "File responses and mounts serve exact bytes, discover changes, and preserve routing"
  (with-static-tree (root base)
    (declare (ignore base))
    (call-with-static-server
     root (lambda (port)
            (dolist (target '("/download" "/assets/hello.txt"))
              (let ((res (file-response port "GET" target)))
                (check-response res "200 OK" (utf8 "héllo ✓"))
                (is (response-header res "content-type") "text/plain")
                (ok (response-header res "last-modified"))))
            (check-response (file-response port "GET" "/assets/sub/data.bin") "200 OK"
                            (static-test-bytes))
            (check-response (file-response port "GET" "/assets/nested/data.bin") "200 OK"
                            (static-test-bytes))
            (check-response (file-response port "GET" "/assets/empty.txt") "200 OK" #())
            (check-response (file-response port "GET" "/assets/special") "200 OK" (utf8 "explicit"))
            (check-response (file-response port "GET" "/assets/a+b.txt") "200 OK" (utf8 "plus"))
            (check-response (file-response port "GET" "/assets/literal%252e.txt") "200 OK" (utf8 "percent"))
            (write-test-file (merge-pathnames "new.txt" root) "new")
            (check-response (file-response port "GET" "/assets/new.txt") "200 OK" (utf8 "new"))
            (write-test-file (merge-pathnames "hello.txt" root) "changed")
            (check-response (file-response port "GET" "/download") "200 OK" (utf8 "changed"))
            (delete-file (merge-pathnames "hello.txt" root))
            (is (wire-response-status (file-response port "GET" "/download")) "HTTP/1.1 404 Not Found")))))

 (subtest
  "Traversal and malformed encodings cannot escape a mounted directory"
  (with-static-tree (root base)
    (declare (ignore base))
    (call-with-static-server
     root (lambda (port)
            (dolist (target '("/assets" "/assets/" "/assets/sub" "/assets-evil/hello.txt"
                              "/assets/../secret.txt" "/assets/%2e%2e/secret.txt"
                              "/assets/%2e%2e%2fsecret.txt" "/assets/sub/%2e%2e/%2e%2e/secret.txt"
                              "/assets/%252e%252e/secret.txt" "/assets/%2fsecret.txt"
                              "/assets/%5c..%5csecret.txt" "/assets/C%3a/secret.txt"
                              "/assets/hello.txt%00" "/assets/%GG" "/assets/%FF"
                              "/assets/hello.txt%" "/assets/hello.txt%2"))
              (is (wire-response-status (file-response port "GET" target)) "HTTP/1.1 404 Not Found" target))))))

 (subtest
  "HEAD, revalidation, invalid validators, and method restrictions"
  (with-static-tree (root base)
    (declare (ignore base))
    (call-with-static-server
     root (lambda (port)
            (let* ((get (file-response port "GET" "/assets/hello.txt"))
                   (date (response-header get "last-modified"))
                   (head (file-response port "HEAD" "/assets/hello.txt"))
                   (cached (file-response port "GET" "/assets/hello.txt" "If-Modified-Since" date)))
              (is (wire-response-status head) "HTTP/1.1 200 OK")
              (is (response-header head "content-length") (response-header get "content-length"))
              (is (length (wire-response-body head)) 0)
              (is (wire-response-status cached) "HTTP/1.1 304 Not Modified")
              (is (wire-response-status (file-response port "GET" "/reused" "If-Modified-Since" date))
                  "HTTP/1.1 304 Not Modified")
              (is (wire-response-status (file-response port "GET" "/reused")) "HTTP/1.1 200 OK"
                  "Conditional response does not mutate a reused descriptor")
              (is (response-header cached "last-modified") date)
              (is (response-header cached "content-length") nil)
              (is (length (wire-response-body cached)) 0)
              (dolist (invalid '("garbage" "Mon, 31 Feb 2020 00:00:00 GMT"))
                (is (wire-response-status (file-response port "GET" "/download" "If-Modified-Since" invalid))
                    "HTTP/1.1 200 OK"))
              (is (wire-response-status (file-response port "GET" "/download" "If-Modified-Since"
                                                      "Thu, 01 Jan 1970 00:00:00 GMT")) "HTTP/1.1 200 OK")
              (is (wire-response-status (file-response port "GET" "/download" "If-None-Match" "*"))
                  "HTTP/1.1 304 Not Modified")
              (let ((res (file-response port "POST" "/assets/hello.txt")))
                (is (wire-response-status res) "HTTP/1.1 405 Method Not Allowed")
                (is (response-header res "allow") "GET, HEAD"))
              (let ((missing (file-response port "HEAD" "/assets/missing")))
                (is (wire-response-status missing) "HTTP/1.1 404 Not Found")
                (is (length (wire-response-body missing)) 0)))))))

 (subtest
  "Bounded property: parent components cannot resolve outside the tree"
  (with-static-tree (root base)
    (declare (ignore base))
    (let ((*num-trials* 20))
      (for-all ((depth (lambda () (1+ (random 8)))))
        (is= (resolve-test-file root (format nil "~{~a/~}secret.txt"
                                            (loop repeat depth collect ".."))) nil))))))
