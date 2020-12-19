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

(defun write-response! (res stream)
  (write-ln stream "HTTP/1.1 " (response-code res))
  (write-ln stream "Content-Type: " (content-type res) "; charset=" (charset res))
  (loop for (name . value) in (headers res)
     when (not (member
		name '("Content-Type" "Set-Cookie" "Location" "Connection" "Expires" "Content-Length")
		:test-not #'string/=))
     do (write-ln stream name ": " value))
  (awhen (cookie res)
    (if (null *cookie-domains*)
	(write-ln stream "Set-Cookie: name=" it)
	(loop for d in *cookie-domains*
	   do (write-ln stream "Set-Cookie: name=" it "; domain=" d))))
  (awhen (location res)
    (write-ln stream "Location: " it))
  (when (keep-alive? res)
    (write-ln stream "Connection: keep-alive")
    (write-ln stream "Expires: Thu, 01 Jan 1970 00:00:01 GMT"))
  (awhen (body res)
    (write-ln stream "Content-Length: " (write-to-string (length it)))
    #-windows(crlf stream)
    #+windows(format stream "~%")
    (write-ln stream it))
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
