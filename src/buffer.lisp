(in-package :house)

(defstruct (buffer (:constructor make-buffer (bi-stream)))
  (tries 0 :type integer)
  (contents nil)
  (bi-stream nil)
  (total-buffered 0 :type integer)
  (started (get-universal-time))
  (request nil)
  (expecting 0 :type integer))

(defun headers-terminated? (octets)
  "OCTETS is the reversed buffer; true when it ends in CR LF CR LF."
  (starts-with-subseq '(10 13 10 13) octets))

(defun buffer! (buffer)
  "Reads whatever is available on BUFFER's stream without blocking.
Returns :EOF if the peer is gone, :BAD if the request can't be parsed,
and otherwise something true."
  (let ((stream (buffer-bi-stream buffer))
        (got-any? nil))
    (flet ((take (octet)
             (setf got-any? t)
             (push octet (buffer-contents buffer))
             (incf (buffer-total-buffered buffer))
             (when (buffer-request buffer)
               (decf (buffer-expecting buffer))))
           (header-done? ()
             (and (null (buffer-request buffer))
                  (headers-terminated? (buffer-contents buffer))))
           (body-done? ()
             (and (buffer-request buffer)
                  (<= (buffer-expecting buffer) 0))))
      (handler-case
          (flet ((drain ()
                   (loop until (or (header-done?)
                                   (body-done?)
                                   (> (buffer-total-buffered buffer) +max-request-size+)
                                   (not (listen stream)))
                         do (take (read-byte stream)))))
            ;; WAIT-FOR-INPUT said this socket is readable. If nothing is
            ;; buffered, a single READ-BYTE won't block: it either gets data
            ;; or sees EOF (LISTEN alone returns NIL at EOF on SBCL, so it
            ;; can't tell a closed socket from a quiet one).
            (unless (listen stream)
              (let ((b (read-byte stream nil :eof)))
                (if (eq b :eof)
                    (return-from buffer! :eof)
                    (take b))))
            (drain)
            (when (header-done?)
              (handler-case
                  (multiple-value-bind (parsed expected) (parse-buffer buffer)
                    (setf (buffer-request buffer) parsed
                          (buffer-expecting buffer) expected
                          (buffer-contents buffer) nil))
                (error () (return-from buffer! :bad)))
              ;; Some or all of the body may already be buffered.
              (drain)))
        (error () (return-from buffer! :eof)))
      ;; Only count wake-ups that produced nothing; a large body legitimately
      ;; arrives over many reads. Slow-drip clients are bounded by
      ;; +MAX-REQUEST-AGE+ instead.
      (unless got-any? (incf (buffer-tries buffer)))
      t)))

(defun json-content-type? (content-type)
  (and (stringp content-type)
       (string-equal "application/json"
                     (string-trim " " (subseq content-type 0 (position #\; content-type))))))

(defun parse-buffer (buf)
  (let ((str (flex:octets-to-string
              (coerce (reverse (buffer-contents buf)) '(vector octet))
              :external-format :utf-8)))
    (if (buffer-request buf)
	(if (json-content-type? (cdr (assoc :content-type (headers (buffer-request buf)))))
	    (cl-json:decode-json-from-string str)
	    (parse-param-string str))
	(parse-request-string str))))
