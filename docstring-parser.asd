;;;; docstring-parser.asd

(asdf:defsystem #:docstring-parser
  :serial t
  :description "Common Lisp docstring parser"
  :author "Mariano Montone"
  :license "MIT"
  :depends-on (#:esrap
               #:alexandria
	       #:cl-ppcre)
  :components ((:file "package")
               (:file "docstring-parser")))
