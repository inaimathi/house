(in-package #:house)

(defclass static-file (response)
  ((relative-path :reader static-file-path :initarg :path)
   (root-directory :reader static-file-root :initarg :root)
   (charset :initform nil)))

(defun static-file (path &key root content-type)
  "Return a file response beneath ROOT. PATH is a decoded, relative filename.
URI decoding belongs to the directory mount, not to this constructor. The file
is resolved and opened when the response is sent; no contents are cached here."
  (unless root (error "STATIC-FILE requires an explicit :ROOT directory."))
  (make-instance 'static-file :path path :root root :content-type content-type))

(defun static-path-components (path)
  ;; Validate before constructing a pathname: avoid native host/device syntax,
  ;; wildcard expansion, Windows alternate streams, and either separator style.
  (let* ((text (etypecase path (string path) (pathname (namestring path))))
         (parts (split-sequence:split-sequence #\/ text)))
    (when (or (zerop (length text))
              (some (lambda (c) (or (< (char-code c) 32) (= (char-code c) 127)
                                   (find c "\\:*?"))) text)
              (some (lambda (part) (member part '("" "." "..") :test #'string=)) parts))
      (return-from static-path-components nil))
    ;; Win32 aliases names with trailing dots/spaces and interprets device names.
    ;; Reject those names everywhere so mounts keep the same policy on all OSes.
    (when (some (lambda (part)
                  (or (find (char part (1- (length part))) ". ")
                      (cl-ppcre:scan "(?i)^(CON|CONIN\\$|CONOUT\\$|CLOCK\\$|PRN|AUX|NUL|COM[0-9]|LPT[0-9])(?:\\.|$)" part)))
                parts)
      (return-from static-path-components nil))
    parts))

(defun static-path-contained-p (path root)
  (flet ((same (a b) #+(or windows win32) (equalp a b)
                       #-(or windows win32) (equal a b)))
    (let ((parent (pathname-directory root)) (child (pathname-directory path)))
      (and (same (pathname-host path) (pathname-host root))
           (same (pathname-device path) (pathname-device root))
           (<= (length parent) (length child))
           (every #'same parent (subseq child 0 (length parent)))))))

(defun resolve-static-file (res)
  (handler-case
      (let* ((parts (static-path-components (static-file-path res)))
             (root (cl-fad:directory-exists-p (static-file-root res))))
        (when (and parts root)
          (let* ((root (truename root))
                 ;; Supplying name/type explicitly keeps dots and other reader
                 ;; syntax in a filename literal rather than interpreting it.
                 (candidate (make-pathname :defaults root
                                           :directory (append (pathname-directory root)
                                                              (butlast parts))
                                           :name (car (last parts)) :type nil :version nil))
                 (target (when (probe-file candidate) (truename candidate))))
            (when (and target (static-path-contained-p target root)
                       (not (cl-fad:directory-exists-p target)))
              target))))
    (file-error () nil)))

(defparameter +http-months+ #("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                             "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
(defparameter +http-weekdays+ #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

(defun http-date (time)
  (multiple-value-bind (second minute hour day month year weekday)
      (decode-universal-time time 0)
    (format nil "~a, ~2,'0d ~a ~4,'0d ~2,'0d:~2,'0d:~2,'0d GMT"
            (aref +http-weekdays+ weekday) day (aref +http-months+ (1- month))
            year hour minute second)))

(defun parse-http-date (text)
  ;; Accept IMF-fixdate and the two obsolete HTTP date formats. Invalid dates
  ;; are ignored, including calendar dates ENCODE-UNIVERSAL-TIME normalizes.
  (handler-case
      (when text
        (multiple-value-bind (match fields)
            (cl-ppcre:scan-to-strings
             "^[A-Za-z]+, ([0-9]{2})[ -]([A-Za-z]{3})[ -]([0-9]{2,4}) ([0-9]{2}):([0-9]{2}):([0-9]{2}) GMT$" text)
          (unless match
            (multiple-value-bind (matched alternative)
                (cl-ppcre:scan-to-strings
                 "^[A-Za-z]{3} ([A-Za-z]{3}) +([0-9]{1,2}) ([0-9]{2}):([0-9]{2}):([0-9]{2}) ([0-9]{4})$" text)
              (unless matched (return-from parse-http-date nil))
              (setf fields (vector (aref alternative 1) (aref alternative 0)
                                   (aref alternative 5) (aref alternative 2)
                                   (aref alternative 3) (aref alternative 4)))))
          (let* ((day (parse-integer (aref fields 0)))
                 (month (1+ (position (aref fields 1) +http-months+ :test #'string=)))
                 (year (parse-integer (aref fields 2)))
                 (hour (parse-integer (aref fields 3)))
                 (minute (parse-integer (aref fields 4)))
                 (second (parse-integer (aref fields 5))))
            (when (= 2 (length (aref fields 2)))
              (let ((current-year (nth-value 5 (decode-universal-time (get-universal-time) 0))))
                (incf year (* 100 (floor current-year 100)))
                (when (> year (+ current-year 50)) (decf year 100))))
            (let ((time (encode-universal-time second minute hour day month year 0)))
              (when (equal (subseq (multiple-value-list (decode-universal-time time 0)) 0 6)
                           (list second minute hour day month year))
                time)))))
    (error () nil)))

(defmethod write-response! ((res static-file) stream &optional request)
  (let ((method (if request (http-method request) :get)))
    (unless (member method '(:get :head))
      (return-from write-response!
        (write-response! (make-instance 'response :response-code "405 Method Not Allowed"
                                       :headers '(("Allow" . "GET, HEAD"))
                                       :body "Only GET and HEAD are supported.") stream request)))
    ;; Resolve and open before sending any headers. A missing/inaccessible file
    ;; returns 404 rather than leaking filesystem details or a partial 200.
    (let* ((path (resolve-static-file res))
           (file (when path
                   (handler-case (open path :direction :input :element-type 'octet)
                     (file-error () nil)))))
      (unless file (return-from write-response! (write-response! +404+ stream request)))
      (unwind-protect
           (let* ((size (file-length file))
                  (modified (file-write-date file))
                  (modified (when modified (min modified (get-universal-time))))
                  (incoming (when request (headers request)))
                  (none-match (assoc :if-none-match incoming))
                  (since (unless none-match (parse-http-date (cdr (assoc :if-modified-since incoming)))))
                  (unchanged (or (and none-match (string= "*" (string-trim " " (cdr none-match))))
                                 (and modified since (<= modified since)))))
             (let ((reply (make-instance
                           'response :response-code (if unchanged "304 Not Modified" "200 OK")
                           :cookie (cookie res)
                           :content-type (or (content-type res)
                                             (cdr (assoc (pathname-type path) *mimetype-table*
                                                         :test #'string-equal))
                                             "application/octet-stream")
                           :headers (append (when modified (list (cons "Last-Modified" (http-date modified))))
                                            '(("Cache-Control" . "no-cache")
                                              ("X-Content-Type-Options" . "nosniff"))))))
               (setf (charset reply) nil)
               (write-response-headers! reply stream (unless unchanged size)))
             (unless (or unchanged (eq method :head))
               (handler-case
                   (let ((buffer (make-array 16384 :element-type 'octet)))
                     ;; Send only the size we advertised, even if the file grows.
                     (loop with remaining = size while (plusp remaining)
                           for count = (read-sequence buffer file :end (min remaining (length buffer)))
                           do (when (zerop count) (return))
                              (write-sequence buffer stream :end count)
                              (decf remaining count)))
                 ;; Once headers are sent, close a failed transfer; never append
                 ;; a second HTTP response to the partially transmitted body.
                 (error () nil))))
        (close file)))))
