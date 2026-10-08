(in-package :house)

(defclass response ()
  ((content-type :accessor content-type :initform "text/html" :initarg :content-type)
   (headers :accessor headers :initarg :headers :initform nil)
   (charset :accessor charset :initform "utf-8")
   (response-code :accessor response-code :initform "200 OK" :initarg :response-code)
   (cookie :accessor cookie :initform nil :initarg :cookie)
   (location :accessor location :initform nil :initarg :location)
   (cache-control :accessor cache-control :initform nil)
   (keep-alive? :accessor keep-alive? :initform nil :initarg :keep-alive?)
   (expires :accessor expires :initform nil)
   (body :accessor body :initform nil :initarg :body)))

(defun write-response-headers! (res stream length)
  (write-ln stream "HTTP/1.1 " (response-code res))
  (write-ln stream "Content-Type: " (content-type res)
            (if (charset res) (format nil "; charset=~a" (charset res)) ""))
  (loop for (name . value) in (headers res)
        unless (member name '("Content-Type" "Set-Cookie" "Location" "Connection"
                              "Expires" "Content-Length") :test #'string-equal)
          do (write-ln stream name ": " value))
  (awhen (cookie res)
    (if (null *cookie-domains*)
        (write-ln stream "Set-Cookie: name=" it)
        (loop for d in *cookie-domains*
              do (write-ln stream "Set-Cookie: name=" it "; domain=" d))))
  (awhen (location res) (write-ln stream "Location: " it))
  (write-ln stream "Connection: " (if (keep-alive? res) "keep-alive" "close"))
  (when (keep-alive? res) (write-ln stream "Expires: Thu, 01 Jan 1970 00:00:01 GMT"))
  (when length (write-ln stream "Content-Length: " (write-to-string length)))
  (crlf stream))

;; Allow reloading this upgrade into a REPL that loaded the old ordinary function.
(eval-when (:load-toplevel :execute)
  (when (and (fboundp 'write-response!)
             (not (typep (fdefinition 'write-response!) 'generic-function)))
    (fmakunbound 'write-response!)))

(defgeneric write-response! (res stream &optional request))

(defmethod write-response! ((res response) stream &optional request)
  (let ((octets (etypecase (body res)
                  (null nil)
                  (string (flex:string-to-octets (body res) :external-format :utf-8))
                  (vector (body res)))))
    (write-response-headers! res stream (when octets (length octets)))
    (unless (and request (eq :head (http-method request)))
      (when octets (write-sequence octets stream))))
  (values))

;;;;;;;;;; HTTP basic responses
(defun redirect! (target &key permanent?)
  (make-instance
   'response
   :response-code (if permanent?
		      "301 Moved Permanently"
		      "307 Temporary Redirect")
   :location target :content-type "text/plain"
   :body "Resource moved..."))

(defparameter +404+
  (make-instance 'response :response-code "404 Not Found"
		 :content-type "text/plain" :body "Resource not found..."))

(defparameter +400+
  (make-instance 'response :response-code "400 Bad Request"
		 :content-type "text/plain" :body "Malformed, or slow HTTP request..."))

(defparameter +413+
  (make-instance 'response :response-code "413 Request Entity Too Large"
		 :content-type "text/plain" :body "Your request is too long..."))

(defparameter +500+
  (make-instance 'response :response-code "500 Internal Server Error"
		 :content-type "text/plain" :body "Something went wrong on our end..."))
