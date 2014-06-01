(defpackage docstring-parser.test
  (:use :cl :docstring-parser :esrap :stefil)
  (:export :run-tests))

(in-package :docstring-parser.test)

(defun run-tests ()
  (without-debugging
    (docstring-parser-test)))

(in-root-suite)

(defsuite docstring-parser-test)

(in-suite docstring-parser-test)

(deftest newline-test ()
  (let ((newline "
"))
    (parse 'docstring-parser::eol newline))

  (signals error
    (let ((newline "lala"))
      (parse 'docstring-parser::eol newline))))

(deftest spacing-test ()

  (let ((spaces "     "))
    (parse 'docstring-parser::spacing spaces))

  (let ((spaces (coerce '(#\Tab #\Tab #\  ) 'string)))
    (parse 'docstring-parser::spacing spaces))

  (signals error
    (let ((spaces "
                  "))
      (parse 'docstring-parser::spacing spaces))))

(deftest spacing*-test ()

  (let ((spaces "     "))
    (parse 'docstring-parser::spacing* spaces))

  (let ((spaces (coerce '(#\Tab #\Tab #\  ) 'string)))
    (parse 'docstring-parser::spacing* spaces))

  (let ((spaces "
                  "))
    (parse 'docstring-parser::spacing* spaces))

  (signals error
    (let ((text "    asfasdf  "))
      (parse 'docstring-parser::spacing* text))))

(deftest word-test ()
  (let ((word "hello"))
    (parse 'docstring-parser::word word))

  (signals error
    (let ((word "hello "))
      (parse 'docstring-parser::word word)))

  (let ((word "**-asdf_%asdf"))
    (parse 'docstring-parser::word word)))

(deftest text-test ()

  (let ((text "asdf asdf; hdsf"))
    (parse 'docstring-parser::text text))

  (let ((text "     asdfasdf  "))
    (parse 'docstring-parser::text text))

  (let ((text " asdfasdf
              asdfas"))
    (signals error
      (parse 'docstring-parser::text-line text)))

  (let ((text " asdfasdf
              asdfas"))
    (parse 'docstring-parser::text text)))

(deftest code-test  ()

  (let ((code-element "``(+ 2 `(print ,'asdf))``"))
    (parse 'docstring-parser::code-element code-element))

  (let ((code-element "``(let ((x 22))
                             (print x))``"))
    (parse 'docstring-parser::code-element code-element)))

(deftest bold-test ()
  (is
   (equalp
    (let ((bold-element "**this is bold**"))
      (parse 'docstring-parser::bold-element bold-element))
    (docstring-parser::make-bold-element :text "this is bold")))
  (is
   (equalp
    (let ((bold-element "**this is
bold**"))
      (parse 'docstring-parser::bold-element bold-element))
    (docstring-parser::make-bold-element :text "this is
bold")))
  )

(deftest italic-test ()
  (is
   (equalp
    (let ((italic-element "//this is italic//"))
      (parse 'docstring-parser::italic-element italic-element))
    (docstring-parser::make-italic-element :text "this is italic")))
  (is
   (equalp
    (let ((italic-element "//this is
italic//"))
      (parse 'docstring-parser::italic-element italic-element))
    (docstring-parser::make-italic-element :text "this is
italic")))
  (signals error
    (parse 'docstring-parser::italic-element "//asdf")))

(deftest list-test ()

  (let ((list-item "* my item"))
    (parse 'docstring-parser::list-item list-item))

  (let ((list-item "* my item
                    has lots of lines!!"))
    (parse 'docstring-parser::list-item list-item))

  (let ((list-item "      * an item"))
    (parse 'docstring-parser::list-item list-item))

  (let ((list-item "   * * an item"))
    (parse 'docstring-parser::list-item list-item))

  (let ((list-item "* item with **markup** //baby//"))
    (parse 'docstring-parser::list-item list-item))

  (let ((list "* first item
             * second item"))
    (parse 'docstring-parser::list-element list))

  (let ((list "* first item
               asdfasdfasdf
               asdfasdfsf
             * second item
             * third * third"))
    (parse 'docstring-parser::list-element list)))

(deftest markup-text-test ()
  (is (tree-equal
       (let ((text "this is a **test**"))
	 (parse 'docstring-parser::markup-text text))
       (list "this is a " (docstring-parser::make-bold-element :text "test"))
       :test #'equalp))

  (is (tree-equal
       (let ((text "this is a //super// **test**"))
	 (parse 'docstring-parser::markup-text text))
       (list "this is a "
	     (docstring-parser::make-italic-element :text "super")
	     " "
	     (docstring-parser::make-bold-element :text "test"))
       :test #'equalp))

  (is (tree-equal
       (let ((text "this ``is a //super// **test**`` of **code**"))
	 (parse 'docstring-parser::markup-text text))
       (list "this "
	     (docstring-parser::make-code-element :text "is a //super// **test**")
	     " of "
	     (docstring-parser::make-bold-element :text "code"))
       :test #'equalp)))

(deftest docstring-options-test ()
  (is
   (equalp
    (let ((text "my-option:true"))
      (parse 'docstring-parser::docstring-option text))
    (docstring-parser::make-docstring-option-element :name "my-option"
						     :value "true")))

  (is (equalp
       (parse 'docstring-parser::docstring-option "lisp")
       (docstring-parser::make-docstring-option-element :name "lisp")))

  (is (equalp
       (parse 'docstring-parser::docstring-options "!opt1: value1; opt2: value2;")
       (docstring-parser::make-docstring-options-element
	:options (list (docstring-parser::make-docstring-option-element
			:name "opt1"
			:value "value1")
		       (docstring-parser::make-docstring-option-element
			:name "opt2"
			:value "value2"))))))

(deftest reference-test ()
  (is (equalp
       (parse 'docstring-parser::reference "MY-REF")
       (docstring-parser::make-ref-element :name "MY-REF")))
  (is (equalp
       (parse 'docstring-parser::reference "`my-ref`")
       (docstring-parser::make-ref-element :name "my-ref")))
  (is (equalp
       (parse 'docstring-parser::reference "`my-ref`(function)")
       (docstring-parser::make-ref-element :name "my-ref"
					   :type "function")))
  (signals error
    (parse 'docstring-parser::reference "`my-ref"))
  (signals error
    (parse 'docstring-parser::reference "my-ref"))
  (signals error
      (parse 'docstring-parser::reference "MY-REF(function)")))

(deftest output-normalization-test ()

  ;; Concat
  (is
   (equalp (docstring-parser::concat-inbetween-text '("foo" "bar"))
	   (list "foobar")))

  (is (equalp (docstring-parser::concat-inbetween-text '("foo" "bar" (:li "lala")))
	      (list "foobar" '(:li "lala"))))

  (is (equalp (docstring-parser::concat-inbetween-text '("foo" (:li "lala") "bar" ))
	      '("foo" (:li "lala") "bar" )))

  ;; Full Normalization
  (is
   (equalp (docstring-parser::normalize-markup-text
	    (list "this "
		  (docstring-parser::make-bold-element :text "is")
		  " cool"))
	   (list "this "
		  (docstring-parser::make-bold-element :text "is")
		  " cool")))
  (is
   (equalp (docstring-parser::normalize-markup-text
	    (list "this "
		  (docstring-parser::make-bold-element :text "is")
		  " not so"
		  " cool"))
	   (list "this "
		  (docstring-parser::make-bold-element :text "is")
		  " not so cool"))))
