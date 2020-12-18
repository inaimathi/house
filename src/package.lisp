(defpackage :house
  (:use :cl #:optima #:cl-ppcre #:usocket #:lisp-unit)
  (:import-from #:alexandria :starts-with-subseq :with-gensyms)
  (:import-from #:flexi-streams :octet)
  (:import-from #:anaphora :aif :awhen :aand :it)
  (:export
   :with-handler-table :empty-handler-table :insert-handler! :find-handler

   :closing-handler :stream-handler :redirect!
   :define-channel :define-handler :define-json-handler :define-file-handler

   :>>string :>>integer :>>keyword :>>json :>>list

   :request :resource :headers :parameters

   :assert-http :root :session :parameters :socket-of

   :new-session! :new-session-hook! :clear-session-hooks! :get-session!
   :subscribers-of :subscribe! :publish! :make-sse

   :lookup

   :start))

(in-package :house)

(declaim (inline crlf write-ln idling? flex-stream write-response! write-sse! process-ready parse-param-string parse-request-string))

(setf *random-state* (make-random-state t))

(defparameter *cookie-domains* nil)

(defparameter +max-request-size+ 50000)
(defparameter +max-buffer-tries+ 10)
(defparameter +max-request-age+ 30)

(defparameter +max-session-idle+ (* 30 60))
(defparameter +clean-sessions-every+ 10000)
