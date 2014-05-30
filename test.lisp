(defpackage docstring-parser.test
  (:use :cl :docstring-parser :esrap :stefil))

(in-package :docstring-parser.test)

(let ((newline "
"))
  (parse 'docstring-parser::eol newline))

(signals error
  (let ((newline "lala"))
    (parse 'docstring-parser::eol newline)))

;; spacing

(let ((spaces "     "))
  (parse 'docstring-parser::spacing spaces))

(let ((spaces (coerce '(#\Tab #\Tab #\  ) 'string)))
  (parse 'docstring-parser::spacing spaces))

(signals error
  (let ((spaces "
                  "))
  (parse 'docstring-parser::spacing spaces)))

;; spacing*

(let ((spaces "     "))
  (parse 'docstring-parser::spacing* spaces))

(let ((spaces (coerce '(#\Tab #\Tab #\  ) 'string)))
  (parse 'docstring-parser::spacing* spaces))

(let ((spaces "
                  "))
  (parse 'docstring-parser::spacing* spaces))

(signals error
  (let ((text "    asfasdf  "))
    (parse 'docstring-parser::spacing* text)))

(let ((word "hello"))
  (parse 'docstring-parser::word word))

(signals error
 (let ((word "hello "))
  (parse 'docstring-parser::word word)))

(let ((word "**-asdf_%asdf"))
  (parse 'docstring-parser::word word))

;; Text

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
  (parse 'docstring-parser::text text))

;; Code

(let ((code-element "``(+ 2 `(print ,'asdf))``"))
  (parse 'docstring-parser::code-element code-element))

(let ((code-element "``(let ((x 22))
                             (print x))``"))
  (parse 'docstring-parser::code-element code-element))

;; Lists

(let ((list-item "* my item"))
  (parse 'docstring-parser::list-item list-item))

(let ((list-item "* my item
                    has lots of lines!!"))
   (parse 'docstring-parser::list-item list-item))

(let ((list-item "      * an item"))
  (parse 'docstring-parser::list-item list-item))

(let ((list-item "   * * an item"))
  (parse 'docstring-parser::list-item list-item))

(let ((list "* first item
             * second item"))
  (parse 'docstring-parser::list-element list))


(let ((list "* first item
               asdfasdfasdf
               asdfasdfsf
             * second item
             * third * third"))
  (parse 'docstring-parser::list-element list))

;; Output normalization

(is
 (equalp (docstring-parser::concat-inbetween-text '("foo" "bar"))
	 (list "foobar")))

(is (equalp (docstring-parser::concat-inbetween-text '("foo" "bar" (:li "lala")))
	    (list "foobar" '(:li "lala"))))

(is (equalp (docstring-parser::concat-inbetween-text '("foo" (:li "lala") "bar" ))
	    '("foo" (:li "lala") "bar" )))
