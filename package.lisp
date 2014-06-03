;;;; package.lisp

(defpackage #:docstring-parser
  (:use #:cl #:esrap #:alexandria #:annot.class #:annot.doc)
  (:export
   ;; Parsing api
   #:parse-function-docstring
   #:parse-class-docstring
   #:parse-class-slot-docstring
   #:parse-package-docstring

   ;; Metadata accessing
   #:docstring-metadata-author
   #:docstring-metadata-todo
   #:docstring-metadata-tags
   #:docstring-metadata-categories
   #:docstring-metadata-version
   #:docstring-metadata-date
   #:docstring-metadata-see))
