;;;; house.asd

(asdf:defsystem #:house
  :description "Custom asynchronous HTTP server for the Deal project."
  :author "Inaimathi <leo.zovic@gmail.com>"
  :license "AGPL3"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria
	       #:anaphora

	       #:bordeaux-threads #:usocket #:flexi-streams
	       #:cl-fad #:cl-ppcre #:optima #:cl-json #:split-sequence
	       #:quri

	       #:session-token #:trivial-features

	       #:lisp-unit)
  :components ((:module
		src :components
		((:file "package")
		 (:file "model")
		 (:file "handler-table")
		 (:file "util")
		 (:file "define-handler")
		 (:file "session")
		 (:file "house")))))

(asdf:defsystem #:house/test
  :description "Test suite for :house"
  :author "Inaimathi <leo.zovic@gmail.com>"
  :license "AGPL3"
  :serial t
  :depends-on (#:house #:test-utils)
  :defsystem-depends-on (#:prove-asdf)
  :components ((:module
                test :components
                ((:file "package")
                 (:test-file "house"))))
  :perform (test-op
	    :after (op c)
	    (funcall (intern #.(string :run) :prove) c)))
