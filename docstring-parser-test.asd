(asdf:defsystem #:docstring-parser-test
  :serial t
  :description "Common Lisp docstring parser test"
  :author "Mariano Montone"
  :license "MIT"
  :depends-on (#:docstring-parser
               #:stefil)
  :components ((:file "test")))
