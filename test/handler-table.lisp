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
      (with-handler-table (empty-handler-table)
	(insert-handler! :any "/a" :foo)
	(find-handler :GET "/a"))
      "Finds a handler")
  (is :foo
      (with-handler-table (empty-handler-table)
	(insert-handler! :any "/a/b/c/d/e/f" :foo)
	(find-handler :GET "/a/b/c/d/e/f"))
      "Finds a deep handler")

  (is '(:FOO ((:C . "see")))
      (multiple-value-list
       (with-handler-table (empty-handler-table)
	 (insert-handler! :any 'a/b/<c>=>>string :foo)
	 (find-handler :GET "/a/b/see")))
      "Finds a handler with a variable in its path and binds")

  (is '(:FOO ((:C . "See")))
      (multiple-value-list
       (with-handler-table (empty-handler-table)
	 (insert-handler! :any 'a/b/<c>=>>string/d/e :foo)
	 (find-handler :GET "/a/b/See/d/e")))
      "Finds a handler with a variable in the middle of its path and binds")

  (is '(nil)
      (multiple-value-list
       (with-handler-table (empty-handler-table)
	 (insert-handler! :any 'a/b/<c>=>>string/d/e :foo)
	 (find-handler :GET "/a/b/see")))
      "Rejects incomplete paths, if there are remaining elements past the variable")

  (is '(:FOO ((:C . "SEE")))
      (multiple-value-list
       (with-handler-table (empty-handler-table)
	 (insert-handler! :any 'a/b/<c>=>>string/d/e :foo)
	 (find-handler :GET "/a/b/SEE/d/e")))
      "Finds a handler with a variable in the middle of its path and binds")

  (is '(:FOO ((:C . "see") (:A . "aye")))
      (multiple-value-list
       (with-handler-table (empty-handler-table)
	 (insert-handler! :any '<a>=>>string/b/<c>=>>string/d/e :foo)
	 (find-handler :GET "/aye/b/see/d/e")))
      "Finds a handler with multiple variables of its path and binds")

  (is '(:FOO ((:C . "See") (:B . "Bee") (:A . "ayE")))
      (multiple-value-list
       (with-handler-table (empty-handler-table)
	 (insert-handler! :any '<a>=>>string/<b>=>>string/<c>=>>string/d/e :foo)
	 (find-handler :GET "/ayE/Bee/See/d/e")))
      "Path variables are bound in a handler call are case sensitive")

  (is '(:BAR nil)
      (with-handler-table (empty-handler-table)
	(insert-handler! :any '<a>=>>string/b/<c>=>>string/d/e :foo)
	(insert-handler! :any 'a/foo/bar/d/e :bar)
	(multiple-value-list (find-handler :GET "/a/foo/bar/d/e")))
      "Finds the correct handler when variables would conflict")

  (is '(:BAR nil)
      (with-handler-table (empty-handler-table)
	(insert-handler! :any '<a>=>>string/b/<c>=>>string/d/e :foo)
	(insert-handler! :any 'a/b/c/d/e :bar)
	(multiple-value-list (find-handler :GET "/a/b/c/d/e")))
      "Prefers the non-variable path in case of actual collision")

  (subtest
   "*rest arguments"
   (is-error
    (insert-handler! :any "/a/b/<*rest>/c" :foo :handler-table (empty-handler-table))
    'error
    "A rest variable needs to be the last path element declared")
   (is '(:foo ((:*rest "d" "e" "f" "g")))
       (multiple-value-list
	(with-handler-table (empty-handler-table)
	  (insert-handler! :any "/a/b/<*rest>" :foo)
	  (find-handler :get "/a/b/c/d/e/f/g")))
       "A rest variable is bound to the rest of the path from the point it's declared")
   (is '(:foo ((:*rest "d" "e" "f" "g")))
       (with-handler-table (empty-handler-table)
	 (insert-handler! :any "/a/b/c/d/e/f/g" :bar)
	 (insert-handler! :any "/a/b/<*rest>" :foo)
	 (multiple-value-list
	  (find-handler :get "/a/b/c/d/e/f/g")))
       "Rest handlers shadow deeper handlers")
   (is '(:foo ((:*rest "d" "e" "f" "g")))
       (with-handler-table (empty-handler-table)
	 (insert-handler! :any "/a/b/<see>=>>keyword/d/e/f/g" :bar)
	 (insert-handler! :any "/a/b/<*rest>" :foo)
	 (multiple-value-list
	  (find-handler :get "/a/b/c/d/e/f/g")))
       "Rest handlers shadow deeper variable handlers")
   (is '(:bar nil)
       (with-handler-table (empty-handler-table)
	 (insert-handler! :any "/a/b/c" :bar)
	 (insert-handler! :any "/a/b/<*rest>" :foo)
	 (multiple-value-list
	  (find-handler :get "/a/b/c")))
       "Rest handlers do not shadow peer handlers")
   (is '(:bar ((:see . "c")))
       (with-handler-table (empty-handler-table)
	 (insert-handler! :any "/a/b/<see>=>>keyword" :bar)
	 (insert-handler! :any "/a/b/<*rest>" :foo)
	 (multiple-value-list
	  (find-handler :get "/a/b/c")))
       "Rest handlers do not shadow peer variable handlers"))))
