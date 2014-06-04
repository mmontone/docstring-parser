(defpackage simple-doc
  (:use #:cl #:docstring-parser #:alexandria #:cl-who)
  (:export #:generate-html-doc
	   #:generate-markup-doc))

(in-package :simple-doc)

(defparameter *categories* '(:function :macro :generic-function :slot-accessor :variable :class :condition :constant))

(defun exported-p (sym)
  (cond
    ((consp sym)
     (assert (eql (first sym) 'setf))
     (exported-p (second sym)))
    (t
     (eql (nth-value 1 (find-symbol (symbol-name sym)
				    (symbol-package sym)))
	  :external))))

(defun public-packages ()
  (loop for p in (list-all-packages)
     when (and (has-exported-symbols-p p) (not (eql p (find-package :keyword))))
     collect p))

(defun has-exported-symbols-p (package)
  (do-external-symbols (sym package)
    (declare (ignore sym))
    (return-from has-exported-symbols-p t))
  nil)

(defun needs-documentation (package)
  (loop for what in *categories*
     for names = (names package what)
     when names nconc
       (loop for sym in names unless (docs-for sym what) collect (list sym what))))

(defun names (package what)
  (sort
   (loop for sym being the present-symbols of package
      when (is sym what) collect sym
      when (is `(setf ,sym) what) collect `(setf ,sym))
   #'name<))

(defun name< (n1 n2)
  (cond
    ((and (symbolp n1) (symbolp n2))
     (string< n1 n2))
    ((and (symbolp n1) (listp n2))
     (cond
       ((string< n1 (second n2)) t)
       ((string< (second n2) n1) nil)
       (t t)))
    ((and (listp n1) (symbolp n2))
     (cond
       ((string< (second n1) n2) t)
       ((string< n2 (second n1)) nil)
       (t nil)))
    ((and (listp n1) (listp n2))
     (string< (second n1) (second n2)))))

(defgeneric is (symbol what))
(defgeneric docs-for (symbol what))
(defgeneric pluralization (what))

(defmethod pluralization (what) (format nil "~as" what))

(defmacro define-category (name (symbol what) &body body)
  (let ((is-test (cdr (assoc :is body)))
        (get-docs (cdr (assoc :docs body)))
        (pluralization (cdr (assoc :pluralization body))))
    `(progn
       (defmethod is (,symbol (,what (eql ',name))) ,@is-test)
       (defmethod docs-for (,symbol (,what (eql ',name))) ,@get-docs)
       ,@(when pluralization
               `((defmethod pluralization ((,what (eql ',name)))
                   ,@pluralization))))))

(defun function-p (name)
  (ignore-errors (fdefinition name)))

(defun macro-p (name)
  (and (symbolp name) (macro-function name)))

(defun generic-function-p (name)
  (and (function-p name)
       (typep (fdefinition name) 'generic-function)))

(defun variable-p (name)
  (ignore-errors (boundp name)))

(defun automatic-p (docstring)
  (member docstring '("automatically generated reader method" "automatically generated writer method") :test #'string-equal))

(defun gf-docs (name)
  (let ((simple (documentation (fdefinition name) t))
        (from-setf (and (consp name) (documentation (fdefinition (second name)) t))))

    (or
     (and simple (not (automatic-p simple)) (format nil "The ~a" simple))
     (and from-setf (not (automatic-p from-setf)) (format nil "Set the ~a" from-setf))
     (first (remove-if #'automatic-p (remove nil (mapcar
                         (lambda (m) (documentation m t))
                         (generic-function-methods (fdefinition name)))))))))

(define-category :function (symbol what)
  (:is (and (function-p symbol)
            (not (or (is symbol :macro)
                     (is symbol :generic-function)
                     (is symbol :slot-accessor)))))
  (:docs (documentation symbol 'function)))

(define-category :macro (symbol what)
  (:is (macro-p symbol))
  (:docs (documentation symbol 'function)))

(define-category :generic-function (symbol what)
  (:is (and (generic-function-p symbol)
            (not (is symbol :slot-accessor))))
  (:docs (documentation symbol 'function)))

(define-category :class (symbol what)
  (:is (and (find-class symbol nil) (not (is symbol :condition))))
  (:docs (documentation (find-class symbol) t))
  (:pluralization (format nil "~aes" what)))

(define-category :condition (symbol what)
  (:is (and (find-class symbol nil) (subtypep (find-class symbol nil) 'condition)))
  (:docs (documentation (find-class symbol) t)))

(define-category :variable (symbol what)
  (:is (and (variable-p symbol) (not (is symbol :constant))))
  (:docs   (documentation symbol 'variable)))

(define-category :constant (symbol what)
  (:is (and (variable-p symbol) (constantp symbol)))
  (:docs (documentation symbol 'variable)))

(define-category :slot-accessor (symbol what)
  (:is (and (generic-function-p symbol)
            (some (lambda (m)
                    (or (eql (class-of m) (find-class 'standard-reader-method))
                        (eql (class-of m) (find-class 'standard-writer-method))))
                  (generic-function-methods (fdefinition symbol)))))
  (:docs (gf-docs symbol)))

(defun collect-package-docstrings (package)
  )

(defun generate-markup-doc (output-filename packages))

(defun make-unique-name (thing what)
  (format nil "~A-~A" thing what))

(defun generate-html-doc (output-filename package)
  (with-open-file (stream output-filename
			  :direction :output
			  :if-does-not-exist :create
			  :if-exists :supersede)
    (with-html-output (html stream)
      (htm
       (:html
	 (:head
	  (:title (fmt "Simple doc for ~A" package))
	  (:style
	   (str
	    "div.function{ border-bottom: 1px solid black; }
body {
  font: 12px/1.5 'PT Sans', serif;
  margin: 25px;
}

.tags {
  list-style: none;
  margin: 0;
  overflow: hidden; 
  padding: 0;
}

.tags li {
  float: left; 
}

.tag {
  background: #eee;
  border-radius: 3px 0 0 3px;
  color: #999;
  display: inline-block;
  height: 26px;
  line-height: 26px;
  padding: 0 20px 0 23px;
  position: relative;
  margin: 0 10px 10px 0;
  text-decoration: none;
  -webkit-transition: color 0.2s;
}

.tag::before {
  background: #fff;
  border-radius: 10px;
  box-shadow: inset 0 1px rgba(0, 0, 0, 0.25);
  content: '';
  height: 6px;
  left: 10px;
  position: absolute;
  width: 6px;
  top: 10px;
}

.tag::after {
  background: #fff;
  border-bottom: 13px solid transparent;
  border-left: 10px solid #eee;
  border-top: 13px solid transparent;
  content: '';
  position: absolute;
  right: 0;
  top: 0;
}

.tag:hover {
  background-color: crimson;
  color: white;
}

.tag:hover::after {
   border-left-color: crimson; 
}")))
	 (:body
	  (loop for category in *categories*
		do
		   (htm
		    (:h1 (str (symbol-name category)))
		    (loop for name in (names package category)
			  do
			     (htm (:div :id (make-unique-name name category)
					(:h2 (str name))
					(render-function name stream))))))))))))

(defun render-function (function stream)
  (with-html-output (html stream)
    (htm
     (:div :class "function"
	   ;(:p :class "name" (str function))
	   (when (docs-for function :function)
	     (render-function-docstring
	      (parse-function-docstring
	       (docs-for function :function))
	      stream))))))
	   
(defun render-function-docstring (docstring stream)
  (with-html-output (html stream)
    (htm
     (:div :class "short-description"
	   (render-docstring-markup
	    (function-docstring-short-description docstring)
	    stream))
     (when (function-docstring-args docstring)
       (htm
	(:div :class "arguments"
	      (:p (:b (str "Arguments:")))
	      (:ul
	       (loop for arg in (args-element-args (function-docstring-args docstring))
		  do
		    (htm
		     (:li (:b (str (arg-element-name arg)))
			  (str ": ")
			  (when (arg-element-type arg)
			    (htm (:i (fmt "(~A) " (arg-element-type arg)))))
			  (render-docstring-markup (arg-element-description arg)
						   stream))))))))
     (when (function-docstring-returns docstring)
       (htm
	(:div :class "returns"
	      (:b (str "Returns: "))
	      (render-docstring-markup
	       (returns-element-returns
	       (function-docstring-returns docstring))
	       stream))))
     (when (function-docstring-long-description docstring)
       (htm
	(:div :class "long-description"
	      (render-docstring-markup
	       (function-docstring-long-description docstring)
	       stream))))
     (when (function-docstring-metadata docstring)
       (htm
	(:div :class "metadata"
	      (loop for metadata in (docstring-metadata-metadata
				     (function-docstring-metadata docstring))
		 do (render-docstring-metadata metadata stream))))))))

(defmethod render-docstring-markup ((markup string) stream)
  (with-html-output (html stream)
    (str markup)))

(defmethod render-docstring-markup ((markup cons) stream)
  (loop for elem in markup
       do
       (render-docstring-markup elem stream)))

(defmethod render-docstring-markup ((markup code-element) stream)
  (with-html-output (html stream)
    (:code (str (code-element-text markup)))))

(defmethod render-docstring-markup ((markup bold-element) stream)
  (with-html-output (html stream)
    (:b (str (bold-element-text markup)))))

(defmethod render-docstring-markup ((markup italic-element) stream)
  (with-html-output (html stream)
    (:it (str (italic-element-text markup)))))

(defmethod render-docstring-markup ((markup ref-element) stream)
  
  (with-html-output (html stream)
    (let ((href (format nil "#~A~@[-~A~]"
			(ref-element-name markup)
			(ref-element-type markup))))
    (htm
     (:a :class "reference"
	 :href href
	 (str (ref-element-name markup)))))))

(defmethod render-docstring-markup ((markup email-element) stream)
  (with-html-output (html stream)
    (:a :href (format nil "mailto:~A" (email-element-email markup))
	(str (email-element-email markup)))))

(defmethod render-docstring-markup ((markup link-element) stream)
  (with-html-output (html stream)
    (:a :href (link-element-url markup)
	(str (or (link-element-title markup)
		 (link-element-url markup))))))

(defmethod render-docstring-metadata ((version docstring-version) stream)
  (with-html-output (html stream)
    (htm
     (:div :class "version"
	   (:b "Version: ")
	   (str (docstring-version-version version))))))

(defmethod render-docstring-metadata ((todo docstring-todo) stream)
  (with-html-output (html stream)
    (htm
     (:div :class "todo"
	   (:b "TODO: ")
	   (render-docstring-markup (docstring-todo-todo todo) stream)))))

(defmethod render-docstring-metadata ((see docstring-see) stream)
  (with-html-output (html stream)
    (htm
     (:div :class "see"
	   (:b "See: ")
	   (loop for reference in (docstring-see-references see)
		do
		(render-docstring-markup reference stream))))))

(defmethod render-docstring-metadata ((date docstring-date) stream)
  (with-html-output (html stream)
    (htm
     (:div :class "date"
	   (:b "Date: ")
	   (render-docstring-markup (docstring-date-date date) stream)))))

(defmethod render-docstring-metadata ((author docstring-author) stream)
  (with-html-output (html stream)
    (htm
     (:div :class "author"
	   (:b "Author: ")
	   (render-docstring-markup (docstring-author-author author) stream)))))

(defmethod render-docstring-metadata ((tags docstring-tags) stream)
  (with-html-output (html stream)
    (htm
     (:ul :class "tags"
	  (loop for tag in (docstring-tags-tags tags)
		do
		   (let ((href (format nil "#~A" tag)))
		     (htm
		      (:li 
		       (:a :class "tag"
			   :href href
			   (str tag))))))))))

(defmethod render-docstring-metadata ((categories docstring-categories) stream)
  (with-html-output (html stream)
    (htm
     (:div :class "categories"
	   (:b "Categories: ")
	   (let ((category (first (docstring-categories-categories categories))))
	     (let ((href (format nil "#~A" category)))
	       (htm
		(:a :href href
		    (str category)))))
	   (loop for category in (cdr (docstring-categories-categories categories))
		do
		(let ((href (format nil "#~A" category)))
		  (htm
		   (str ", ")
		   (:a :href href
		       (str category)))))))))
