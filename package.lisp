;;;; package.lisp

(defpackage #:docstring-parser
  (:use #:cl #:esrap #:alexandria)
  (:export #:function-docstring
	   #:function-docstring-short-description
	   #:function-docstring-args
	   #:function-docstring-returns
	   #:function-docstring-long-description
	   #:function-docstring-metadata
	   #:class-docstring
	   #:class-docstring-description
	   #:class-docstring-metadata
	   #:package-docstring
	   #:package-docstring-description
	   #:package-docstring-metadata

	   #:parse-function-docstring
	   #:parse-class-docstring
	   #:parse-class-slot-docstring
	   #:parse-package-docstring))

