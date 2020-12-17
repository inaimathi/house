(in-package #:house/test)

(tests
 (subtest
  "URI Variable destructuring"
  (is (var-key "foo") nil)
  (is (var-annotation "foo") nil)
  (is (var-key "<a>") :a)
  (is (var-annotation "<a>") nil)
  (is (var-key "<foo>") :foo)
  (is (var-annotation "<foo>") nil)
  (is (var-key "<foo>=>>string") :foo)
  (is (var-annotation "<foo>=>>string") '>>string))

 (subtest
  "insert-handler! and find-handler"
  (is nil
      (find-handler :GET "/a/b/c/d" :handler-table (empty-handler-table))
      "Does not find a nonexistent handler")
  (is :foo
      (find-handler
       :GET "/a"
       :handler-table (insert-handler!
		       :any "/a" :foo
		       :handler-table (empty-handler-table)))
      "Finds a handler")
  (is :foo
      (find-handler
       :GET "/a/b/c/d/e/f"
       :handler-table (insert-handler!
		       :any "/a/b/c/d/e/f" :foo
		       :handler-table (empty-handler-table)))
      "Finds a deep handler")

  (is '(:FOO ((:C . "SEE")))
      (multiple-value-list
       (find-handler
	:GET "/a/b/see"
	:handler-table (insert-handler!
			:any 'a/b/<c>=>>string :foo
			:handler-table (empty-handler-table))))
      "Finds a handler with a variable in its path and binds")

  (is '(:FOO ((:C . "SEE")))
      (multiple-value-list
       (find-handler
	:GET "/a/b/see/d/e"
	:handler-table (insert-handler!
			:any 'a/b/<c>=>>string/d/e :foo
			:handler-table (empty-handler-table))))
      "Finds a handler with a variable in the middle of its path and binds")

  (is '(nil)
      (multiple-value-list
       (find-handler
	:GET "/a/b/see"
	:handler-table (insert-handler!
			:any 'a/b/<c>=>>string/d/e :foo
			:handler-table (empty-handler-table))))
      "Rejects incomplete paths, if there are remaining elements past the variable")

  (is '(:FOO ((:C . "SEE")))
      (multiple-value-list
       (find-handler
	:GET "/a/b/see/d/e"
	:handler-table (insert-handler!
			:any 'a/b/<c>=>>string/d/e :foo
			:handler-table (empty-handler-table))))
      "Finds a handler with a variable in the middle of its path and binds")

  (is '(:FOO ((:C . "SEE") (:A . "AYE")))
      (multiple-value-list
       (find-handler
	:GET "/aye/b/see/d/e"
	:handler-table (insert-handler!
			:any '<a>=>>string/b/<c>=>>string/d/e :foo
			:handler-table (empty-handler-table))))
      "Finds a handler with multiple variables of its path and binds")

  (is '(:BAR nil)
      (multiple-value-list
       (find-handler
	:GET "/a/foo/bar/d/e"
	:handler-table
	(with-handler-table (empty-handler-table)
	  (insert-handler!
	   :any '<a>=>>string/b/<c>=>>string/d/e :foo
	   :handler-table (empty-handler-table))
	  (insert-handler!
	   :any 'a/foo/bar/d/e :bar))))
      "Finds the correct handler when variables would conflict")

  (is '(:BAR nil)
      (multiple-value-list
       (find-handler
	:GET "/a/b/c/d/e"
	:handler-table (with-handler-table (empty-handler-table)
			 (insert-handler!
			  :any '<a>=>>string/b/<c>=>>string/d/e :foo
			  :handler-table (empty-handler-table))
			 (insert-handler!
			  :any 'a/b/c/d/e :bar))))
      "Prefers the non-variable path in case of actual collision")))
