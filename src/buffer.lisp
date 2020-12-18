(in-package :house)

(defstruct (buffer (:constructor make-buffer (bi-stream)))
  (tries 0 :type integer)
  (contents nil)
  (bi-stream nil)
  (total-buffered 0 :type integer)
  (started (get-universal-time))
  (request nil)
  (expecting 0 :type integer))

(defun buffer! (buffer)
  (handler-case
      (let ((stream (buffer-bi-stream buffer)))
	(incf (buffer-tries buffer))
	(loop for char = (read-char-no-hang stream)
	   until (or (null char) (eql :eof char))
	   do (push char (buffer-contents buffer))
	   do (incf (buffer-total-buffered buffer))
	   when (buffer-request buffer) do (decf (buffer-expecting buffer))
	   when (and #-windows(char= char #\linefeed)
		     #+windows(char= char #\newline)
		 (line-terminated? (buffer-contents buffer)))
	   do (multiple-value-bind (parsed expecting) (parse-buffer buffer)
		(setf (buffer-request buffer) parsed
		      (buffer-expecting buffer) expecting
		      (buffer-contents buffer) nil)
		(return char))
	   when (> (buffer-total-buffered buffer) +max-request-size+) return char
	   finally (return char)))
    (error () :eof)))

(defun parse-buffer (buf)
  (let ((str (coerce (reverse (buffer-contents buf)) 'string)))
    (if (buffer-request buf)
	(if (eq :application/json (->keyword (cdr (assoc :content-type (headers (buffer-request buf))))))
	    (cl-json:decode-json-from-string str)
	    (parse-param-string str))
	(parse-request-string str))))
