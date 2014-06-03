;;;; package.lisp

(defpackage #:docstring-parser
  (:use #:cl #:esrap #:alexandria #:annot.class #:annot.doc)
  (:export #:parse-function-docstring
	   #:parse-class-docstring
	   #:parse-class-slot-docstring
	   #:parse-package-docstring))
