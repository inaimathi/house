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
  ;; TODO - finds handler
  ;; TODO - finds handler with variables at the end of path
  ;; TODO - finds handler with variables in the middle
  ;; TODO - finds handler with variables at the beginning
  ;; TODO - returns path variables and handler entries
  ;; TODO - does not find nonexistent handlers
  ))
