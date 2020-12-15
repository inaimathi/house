(in-package :house)

(defun split-at (elem seq)
  (split-sequence:split-sequence elem seq :remove-empty-subseqs t))

(defun line-terminated? (lst)
  (starts-with-subseq
   #-windows'(#\linefeed #\return #\linefeed #\return)
   #+windows'(#\newline #\newline)
   lst))

(defun debug! (&optional (stream *standard-output*))
  (flet ((dbg (label &rest msg) (format stream ">>>> ~a~%~{~s~%----------~%~}~%" label msg)))
    (defmethod process-ready :after ((sock stream-server-usocket) conns)
	       (dbg "New listener..." sock
		    "CONNECTIONS: " (alexandria:hash-table-keys conns)))
    (defmethod process-ready :before ((sock stream-usocket) conns)
	       (dbg "Preparing to buffer..." sock
		    "CONNECTIONS: " (alexandria:hash-table-keys conns)))
    (defmethod handle-request :before (sock req)
	       (dbg "Handling request..." sock req (resource req) (headers req) (session-tokens req) (parameters req)))
    (defmethod handle-request :after (sock req)
	       (dbg "Completed request..."))
    (defmethod write! :before ((res response) sock)
	       (dbg "Writing response..."))
    (defmethod error! :before (res sock &optional instance)
	       (dbg "Sending error response..."
		    instance sock res (response-code res)))
    (defmethod subscribe! :before (chan sock)
	       (dbg "New subscriber" chan))
    (defmethod publish! :before (chan msg)
	       (dbg "Publishing to channel" chan msg))
    nil))

(defmethod ->keyword ((thing null)) nil)

(defmethod ->keyword ((thing symbol))
  (intern (symbol-name thing) :keyword))

(defmethod ->keyword ((thing string))
  (intern (string-upcase thing) :keyword))

(defmethod lookup (key (hash hash-table))
  (gethash key hash))

(defmethod lookup (key (session session))
  (gethash key (session-values session)))

(defgeneric (setf lookup) (new-value key session)
  (:documentation "Setter for lookup methods"))

(defmethod (setf lookup) (new-value key (session session))
  (setf (gethash key (session-values session)) new-value))

(defmethod (setf lookup) (new-value key (hash hash-table))
  (setf (gethash key hash) new-value))

(defun flex-stream (sock)
  (flex:make-flexi-stream (socket-stream sock) :external-format :utf-8))

(defmethod path->uri ((path pathname) &key stem-from)
  (format nil "/~{~a/~}~a~@[.~a~]"
	  (if stem-from
	      (member stem-from (cdr (pathname-directory path)) :test #'string=)
	      (cdr (pathname-directory path)))
	  (pathname-name path)
	  (pathname-type path)))

(defparameter *mimetype-table*
  '(("atom" . "application/atom+xml")
    ("bmp" . "image/bmp")
    ("cmc" . "application/vnd.cosmocaller")
    ("css" . "text/css")
    ("gif" . "image/gif")
    ("htm" . "text/html")
    ("html" . "text/html")
    ("ico" . "image/x-icon")
    ("jpe" . "image/jpeg")
    ("jpeg" . "image/jpeg")
    ("jpg" . "image/jpeg")
    ("js" . "application/javascript")
    ("json" . "application/json")
    ("mid" . "audio/midi")
    ("midi" . "audio/midi")
    ("mov" . "video/quicktime")
    ("mp3" . "audio/mpeg")
    ("mp4" . "video/mp4")
    ("mpe" . "video/mpeg")
    ("mpeg" . "video/mpeg")
    ("mpg" . "video/mpeg")
    ("oga" . "audio/ogg")
    ("ogg" . "audio/ogg")
    ("ogv" . "video/ogg")
    ("ogx" . "application/ogg")
    ("png" . "image/png")
    ("tif" . "image/tiff")
    ("tiff" . "image/tiff")
    ("wav" . "audio/x-wav")
    ("xhtml" . "application/xhtml+xml")
    ("xml" . "application/xml")))

(defmethod path->mimetype ((path pathname))
  (aif (cdr (assoc (pathname-type path) *mimetype-table* :test #'string=))
       it
       "text/plain"))
