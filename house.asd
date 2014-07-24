;;;; house.asd

(asdf:defsystem #:house
    :serial t
    :description "Custom asynchronous HTTP server for the Deal project."
  :author "Inaimathi <leo.zovic@gmail.com>"
  :license "AGPL3"
  :depends-on (#:alexandria 
	       #:anaphora 

	       #:bordeaux-threads #:usocket #:flexi-streams
	       #:cl-fad #:cl-ppcre #:optima #:cl-json

	       #:isaac #:session-token

	       #:lisp-unit)
  :components ((:file "package")
	       (:file "model")
	       (:file "util")
	       (:file "define-handler")
	       (:file "session")
	       (:file "house")
	       (:file "unit-tests")))
