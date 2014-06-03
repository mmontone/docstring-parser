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
  #+nil(is (equalp
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

(deftest args-test ()
  (is (equalp
       (parse 'docstring-parser::args-element "Args: -my-arg:This is my arg")
       (docstring-parser::make-args-element
	:args (list (docstring-parser::make-arg-element :name "my-arg"
							:type nil
							:description "This is my arg")))))
  (is (equalp
       (parse 'docstring-parser::args-element "Args: - my-arg:This is my arg
                                                     - another-arg(string): A **string**")
       (docstring-parser::make-args-element
	:args (list (docstring-parser::make-arg-element :name "my-arg"
							:type nil
							:description "This is my arg")
		    (docstring-parser::make-arg-element :name "another-arg"
							:type "string"
							:description (list "A " (docstring-parser::make-bold-element :text "string"))))))))

(deftest commands-test ()
  (is (equalp
       (parse 'docstring-parser::command "\\my-command")
       (docstring-parser::make-command-element :name "my-command")))
  (is (equalp
       (parse 'docstring-parser::command "\\my-command[]")
       (docstring-parser::make-command-element :name "my-command")))
  (is (equalp
       (parse 'docstring-parser::command "@my-command")
       (docstring-parser::make-command-element :name "my-command")))
  (is (equalp
       (parse 'docstring-parser::command "\\my-command[opt1]")
       (docstring-parser::make-command-element :name "my-command"
					       :options (list (list "opt1")))))
  (is (equalp
       (parse 'docstring-parser::command "\\my-command[opt1, opt2=33]")
       (docstring-parser::make-command-element :name "my-command"
					       :options (list (list "opt1")
							      (cons "opt2" "33")))))
  (is (equalp
       (parse 'docstring-parser::command "\\my-command[opt1, opt2=33]{x}")
       (docstring-parser::make-command-element :name "my-command"
					       :options (list (list "opt1")
							      (cons "opt2" "33"))
					       :args (list "x"))))
  (is (equalp
       (parse 'docstring-parser::command "\\my-command[opt1, opt2=33]{x}{y}")
       (docstring-parser::make-command-element :name "my-command"
					       :options (list (list "opt1")
							      (cons "opt2" "33"))
					       :args (list "x" "y")))))

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

(deftest function-docstring-test ()
  (is
   (equalp
    (let ((docstring "A short **description**"))
      (parse 'docstring-parser::function-docstring docstring))
    (docstring-parser::make-function-docstring :short-description
				      (list "A short "
					    (docstring-parser::make-bold-element :text "description")))))

  (is
   (equalp
    (let ((docstring "A short **description**

                      With a long description"))
      (parse 'docstring-parser::function-docstring docstring))
    (docstring-parser::make-function-docstring
     :short-description
     (list "A short "
	   (docstring-parser::make-bold-element :text "description"))
     :long-description "With a long description")))

  
  (let ((docstring "A short **description**

                    With a long description

                    TODO: do this"))
    (parse 'docstring-parser::function-docstring docstring))

  (let ((docstring "A short **description**

                      Args:
                         - hello:Hello"))
    (parse 'docstring-parser::function-docstring docstring))

  (let ((docstring "A short **description**
                          Args:
                            - hello:Hello"))
    (parse 'docstring-parser::function-docstring docstring))

  (let ((docstring "A short **description**

                      With a long description

                      TODO: do this"))
    (parse 'docstring-parser::function-docstring docstring))

  (let ((docstring "A short **description**

                      Args:
                        - my-arg: Thello

                      With a long description
                      ``lala``

                      TODO: do this"))
    (parse 'docstring-parser::function-docstring docstring))

  (let ((docstring "A short **description**

                      Args:
                        - my-arg: Thello

                      TODO: do this
                      Author: Mariano Montone"))
    (parse 'docstring-parser::function-docstring docstring))

  (let ((docstring "A short **description**
                      With more description

                      Args:
                        - my-arg: Thello

                      Look at this example:

                      TODO: do this
                      Author: Mariano Montone"))
    (parse 'docstring-parser::function-docstring docstring))

  (let ((docstring "A short **description**
                      With more description

                      Args:
                        - my-arg: Thello
                        - second (integer): Do this

                      Look at **this** example:
                      ``(+ 2 3)``. Also see: `my-func`(function)

                      This is useful for:

                      * Testing
                      * Prototyping

                      TODO: do this
                      Author: Mariano Montone"))
    (parse 'docstring-parser::function-docstring docstring))

    (let ((docstring "
                      A short **description**
                      With more description

                      Args:
                        - my-arg: Thello
                        - second (integer): Do this

                      Look at **this** example:
                      ``(+ 2 3)``. Also see: `my-func`(function)

                      This is useful for:

                      * Testing \\see[section]{testing}
                      * Prototyping

                      TODO: do this
                      Author: Mariano Montone"))
    (parse 'docstring-parser::function-docstring docstring))


  )
