(in-package #:house/test)

;;;; Real loopback sockets, the production event loop, and no HTTP client
;;;; library that might hide trailing bytes or a premature connection close.

(defun call-with-local-server (setup test)
  (let* ((listener (usocket:socket-listen "127.0.0.1" 0
                                         :element-type '(unsigned-byte 8)))
         (port (usocket:get-local-port listener))
         (table (empty-handler-table))
         (channels (make-hash-table))
         (lock (bordeaux-threads:make-lock "test server"))
         (finished (bordeaux-threads:make-condition-variable))
         (stopping nil) (done nil) (server-error nil) (thread nil))
    (unwind-protect
         (progn
           (with-handler-table table (funcall setup))
           (setf thread
                 (bordeaux-threads:make-thread
                  (lambda ()
                    ;; Special bindings are not inherited by portable threads.
                    (let ((house::*handler-table* table)
                          (house::*channels* channels)
                          (house::*sessions* (make-hash-table :test 'equal))
                          (house::*new-session-hook* nil))
                      (unwind-protect
                           (handler-case
                               (house::serve
                                listener :stop-p
                                (lambda ()
                                  (bordeaux-threads:with-lock-held (lock) stopping)))
                             (error (c) (setf server-error c)))
                        (bordeaux-threads:with-lock-held (lock)
                          (setf done t)
                          (bordeaux-threads:condition-notify finished)))))
                  :name "house end-to-end server"))
           ;; The listener is already bound: no readiness sleeps or free-port
           ;; probe followed by a competing bind are necessary.
           (funcall test port))
      (unwind-protect
           (when thread
             (let ((timed-out nil))
               (bordeaux-threads:with-lock-held (lock)
                 (setf stopping t)
                 (loop until done
                       unless (bordeaux-threads:condition-wait finished lock :timeout 5)
                         do (setf timed-out t) (return)))
               (when timed-out (bordeaux-threads:destroy-thread thread))
               (if timed-out
                   (progn
                     (ignore-errors (bordeaux-threads:join-thread thread))
                     (error "Test server did not shut down within five seconds."))
                   (bordeaux-threads:join-thread thread))))
        ;; Stream subscribers have left SERVE's pending-request table. They are
        ;; owned by this fixture's private channel registry and need closing too.
        (loop for subscribers being the hash-values of channels
              do (dolist (sock subscribers) (ignore-errors (usocket:socket-close sock))))
        (ignore-errors (usocket:socket-close listener)))
      (when server-error (error server-error)))))

(defmacro with-local-server ((port) &body body)
  `(call-with-local-server #'install-test-endpoints (lambda (,port) ,@body)))

(defun install-test-endpoints ()
  (define-handler (hello :content-type "text/plain" :method :get) () "Jérôme ✓")
  (define-handler (echo :content-type "text/plain") ((message >>string) (count >>integer))
    (format nil "~a/~d" message count))
  (define-handler (binary :content-type "application/octet-stream") ()
    (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(0 13 10 255)))
  (define-handler (explode) () (error "A deliberate handler failure"))
  (define-channel (events) () t)
  (define-handler (emit) ((message >>string) (count >>integer))
    (publish! 'events (make-sse message :id count :event "update"))
    "sent"))

(defun call-with-client (port fn &key (timeout 5))
  (let ((sock (usocket:socket-connect "127.0.0.1" port
                                     :element-type '(unsigned-byte 8) :timeout timeout)))
    (unwind-protect (funcall fn sock)
      (ignore-errors (usocket:socket-close sock)))))

(defun utf8 (text)
  (flex:string-to-octets text :external-format :utf-8))

(defun send-octets (sock bytes &key (start 0) end)
  (write-sequence bytes (usocket:socket-stream sock) :start start :end end)
  (finish-output (usocket:socket-stream sock)))

(defun request-bytes (method target &optional (body "") content-type)
  (utf8
   (with-output-to-string (out)
     (flet ((line (text) (format out "~a~c~c" text #\Return #\Linefeed)))
       (line (format nil "~a ~a HTTP/1.1" method target))
       (line "Host: localhost")
       (when content-type
         (line (format nil "Content-Type: ~a" content-type))
         (line (format nil "Content-Length: ~d" (length (utf8 body)))))
       (line "")
       (write-string body out)))))

(defun wire-deadline ()
  (+ (get-internal-real-time) (* 5 internal-time-units-per-second)))

(defun wire-byte (sock deadline)
  (let ((stream (usocket:socket-stream sock)))
    (loop
      (let ((remaining (/ (- deadline (get-internal-real-time))
                          internal-time-units-per-second)))
        (when (<= remaining 0) (error "Timed out reading an HTTP response."))
        (when (or (listen stream)
                  (usocket:wait-for-input sock :timeout remaining :ready-only t))
          (return (read-byte stream nil :eof)))))))

(defun wire-through (sock terminator deadline)
  (let ((bytes (make-array 0 :element-type '(unsigned-byte 8)
                           :adjustable t :fill-pointer 0)))
    (loop for byte = (wire-byte sock deadline)
          do (when (eq byte :eof) (error "Connection closed before ~s." terminator))
             (vector-push-extend byte bytes)
             (when (> (length bytes) 65536) (error "Oversized test response."))
          when (alexandria:ends-with-subseq terminator bytes) return bytes)))

(defstruct wire-response status headers body)

(defun read-wire-response (sock deadline)
  (flet ((line ()
           (let ((bytes (wire-through sock #(13 10) deadline)))
             (flex:octets-to-string (subseq bytes 0 (- (length bytes) 2))
                                   :external-format :utf-8))))
    (let* ((status (line))
           (headers (loop for text = (line) until (string= text "")
                          for colon = (or (position #\: text)
                                          (error "Malformed response header: ~s" text))
                          collect (cons (string-downcase (subseq text 0 colon))
                                        (string-trim " " (subseq text (1+ colon))))))
           (length-header (cdr (assoc "content-length" headers :test #'string=)))
           (size (if length-header (parse-integer length-header) 0)))
      (unless (<= 0 size 65536) (error "Invalid response body length: ~s" size))
      (make-wire-response
       :status status :headers headers
       :body (let ((bytes (make-array size :element-type '(unsigned-byte 8))))
               (dotimes (i size bytes)
                 (let ((byte (wire-byte sock deadline)))
                   (when (eq byte :eof) (error "Truncated response body at byte ~d." i))
                   (setf (aref bytes i) byte))))))))

(defun response-header (response name)
  (cdr (assoc name (wire-response-headers response) :test #'string=)))

(defun closing-response (port bytes)
  (call-with-client
   port (lambda (sock)
          (send-octets sock bytes)
          (let* ((deadline (wire-deadline))
                 (response (read-wire-response sock deadline)))
            ;; Check the actual close, rather than trusting Connection: close.
            (unless (eq :eof (wire-byte sock deadline))
              (error "Unexpected bytes after the response body."))
            response))))

(defun check-response (response status body)
  (is (wire-response-status response) (format nil "HTTP/1.1 ~a" status))
  (is (response-header response "connection") "close")
  (is (response-header response "content-length") (write-to-string (length body)))
  (is (wire-response-body response) body :test #'equalp))

(tests
 (subtest
  "Fixture teardown closes the listener and subscribers even after an error"
  (let ((port nil) (subscriber nil))
    (unwind-protect
         (progn
           (is-error
            (call-with-local-server
             #'install-test-endpoints
             (lambda (p)
               (setf port p
                     subscriber (usocket:socket-connect "127.0.0.1" p
                                                       :element-type '(unsigned-byte 8)
                                                       :timeout 5))
               (send-octets subscriber (request-bytes "GET" "/events"))
               (read-wire-response subscriber (wire-deadline))
               (error "Deliberate fixture-body failure")))
            'simple-error)
           (is (wire-byte subscriber (wire-deadline)) :eof)
           ;; Some nonblocking connect backends report a timeout rather than
           ;; ECONNREFUSED for a closed port. Either means no listener remains.
           (is-error (call-with-client port (lambda (sock) (declare (ignore sock)))
                                       :timeout 0.1)
                     '(or usocket:connection-refused-error usocket:timeout-error)))
      (when subscriber (ignore-errors (usocket:socket-close subscriber))))))

 (subtest
  "Closing responses arrive in full, with exact byte framing and EOF"
  (with-local-server (port)
    (let ((res (closing-response port (request-bytes "GET" "/hello"))))
      (check-response res "200 OK" (utf8 "Jérôme ✓"))
      (is (response-header res "content-type") "text/plain; charset=utf-8"))
    (check-response (closing-response port (request-bytes "GET" "/binary"))
                    "200 OK" #(0 13 10 255))))

 (subtest
  "Query, form, and JSON parameters pass through the real parser and handler"
  (with-local-server (port)
    (dolist (bytes (list (request-bytes "GET" "/echo?message=h%C3%A9llo%20there&count=7")
                        (request-bytes "POST" "/echo"
                                       "message=h%C3%A9llo%20there&count=7"
                                       "application/x-www-form-urlencoded")
                        (request-bytes "POST" "/echo"
                                       "{\"message\":\"héllo there\",\"count\":7}"
                                       "application/json; charset=utf-8")))
      (check-response (closing-response port bytes) "200 OK" (utf8 "héllo there/7")))))

 (subtest
  "An incomplete UTF-8 POST does not block other clients or dispatch early"
  (with-local-server (port)
    (call-with-client
     port (lambda (sock)
            (let* ((bytes (request-bytes "POST" "/echo"
                                          "{\"message\":\"héllo\",\"count\":9}"
                                          "application/json"))
                   ;; Split within the two-byte encoding of é.
                   (split (1+ (position #xc3 bytes))))
              (send-octets sock bytes :end split)
              (check-response (closing-response port (request-bytes "GET" "/hello"))
                              "200 OK" (utf8 "Jérôme ✓"))
              (ok (not (usocket:wait-for-input sock :timeout 0 :ready-only t))
                  "No response before the complete body")
              (send-octets sock bytes :start split)
              (let ((deadline (wire-deadline)))
                (check-response (read-wire-response sock deadline)
                                "200 OK" (utf8 "héllo/9"))
                (is (wire-byte sock deadline) :eof)))))))

 (subtest
  "SSE headers flush before publication; events flush in publication order"
  (with-local-server (port)
    (call-with-client
     port (lambda (sock)
            (send-octets sock (request-bytes "GET" "/events"))
            (let ((res (read-wire-response sock (wire-deadline))))
              (is (wire-response-status res) "HTTP/1.1 200 OK")
              (is (response-header res "content-type") "text/event-stream; charset=utf-8")
              (is (response-header res "connection") "keep-alive")
              (is (response-header res "content-length") nil))
            (loop for message in '("first ✓" "second é") for id from 1
                  do (check-response
                      (closing-response
                       port (request-bytes "GET" (format nil "/emit?message=~a&count=~d"
                                                         (quri:url-encode message) id)))
                      "200 OK" (utf8 "sent")))
            (loop for message in '("first ✓" "second é") for id from 1
                  do (is (wire-through sock #(10 10) (wire-deadline))
                         (utf8 (format nil "id: ~d~%event: update~%data: ~a~%~%" id message))
                         :test #'equalp))))))

 (subtest
  "Error responses flush and a failed request leaves the server usable"
  (with-local-server (port)
    (loop for (bytes status body) in
          (list (list (utf8 (format nil "GET /hello HTTP/1.0~c~c~c~c"
                                    #\Return #\Linefeed #\Return #\Linefeed))
                      "400 Bad Request" "Malformed, or slow HTTP request...")
                (list (request-bytes "POST" "/echo" "{" "application/json")
                      "400 Bad Request" "Malformed, or slow HTTP request...")
                (list (request-bytes "GET" "/echo?message=x&count=nope")
                      "400 Bad Request" "Malformed, or slow HTTP request...")
                (list (request-bytes "GET" "/missing")
                      "404 Not Found" "Resource not found...")
                (list (request-bytes "GET" "/explode")
                      "500 Internal Server Error" "Something went wrong on our end..."))
          do (check-response (closing-response port bytes) status (utf8 body))
             (check-response (closing-response port (request-bytes "GET" "/hello"))
                             "200 OK" (utf8 "Jérôme ✓")))))

 (subtest
  "Bounded property: percent-encoded text and integers round-trip over HTTP"
  (with-local-server (port)
    (let ((*num-trials* 20)
          (text (lambda ()
                  (coerce (loop repeat (random 25)
                                collect (generate (one-of #\a #\Space #\& #\= #\%
                                                          #\+ #\é #\✓)))
                          'string)))
          (integer (lambda () (- (random 2001) 1000))))
      (for-all ((message text) (count integer))
        (is= (let ((res (closing-response
                         port (request-bytes "GET" (format nil "/echo?message=~a&count=~d"
                                                           (quri:url-encode message) count)))))
               (and (string= (wire-response-status res) "HTTP/1.1 200 OK")
                    (equalp (wire-response-body res) (utf8 (format nil "~a/~d" message count)))
                    (string= (response-header res "connection") "close")
                    (string= (response-header res "content-length")
                             (write-to-string (length (wire-response-body res))))))
             t)
        "Twenty bounded generated requests preserve their values and framing")))))
