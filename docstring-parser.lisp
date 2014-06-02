(in-package #:docstring-parser)

(defstruct (list-element
	     (:print-function print-list-element))
  items)

(defun print-list-element (elem stream depth)
  (format stream "(:list ~{~A~})" (list-element-items elem)))

(defstruct (list-item-element
	     (:print-function print-list-item-element))
  text)

(defun print-list-item-element (elem stream depth)
  (format stream "(:li ~S)" (list-item-element-text elem)))

(defstruct (italic-element
	     (:print-function print-italic-element))
  text)

(defun print-italic-element (elem stream depth)
  (format stream "(:italic ~S)" (italic-element-text elem)))

(defstruct (bold-element
	     (:print-function print-bold-element))
  text)

(defun print-bold-element (elem stream depth)
  (format stream "(:bold ~S)" (bold-element-text elem)))

(defstruct (code-element
	     (:print-function print-code-element))
  text)

(defun print-code-element (elem stream depth)
  (format stream "(:code ~S)" (code-element-text elem)))

(defstruct (docstring-option-element
	     (:print-function print-docstring-option-element))
  name
  value)

(defun print-docstring-option-element (elem stream depth)
  (format stream "(:option :name ~S :value ~S)"
	  (docstring-option-element-name elem)
	  (docstring-option-element-value elem)))

(defstruct (docstring-options-element
	     (:print-function print-docstring-options-element))
  options)

(defun print-docstring-options-element (elem stream depth)
  (format stream "(:options ~{~A~})" (docstring-options-element-options elem)))

(defstruct (ref-element
	     (:print-function print-ref-element))
  name
  type)

(defun print-ref-element (elem stream depth)
  (format stream "(:ref~@[ (:type ~A)~] ~S)"
	  (ref-element-type elem)
	  (string-upcase (ref-element-name elem))))

(defstruct (command-element
	     (:print-function print-command-element))
  name
  options
  args)

(defun print-command-element (command stream depth)
  (format stream "(:command ~A [~{~A~}]{~{~A~}})"
	  (command-element-name command)
	  (command-element-options command)
	  (command-element-args command)))

(defun concat-inbetween-text (things)
  (let ((result nil))
    (loop for thing in things
	 do
	 (if (not (stringp thing))
	     (push thing result)
	     ;; else
	     (let ((current-thing (first result)))
	       (if (stringp current-thing)
		   (progn
		     (pop result)
		     (push (concatenate 'string current-thing thing) result))
		   ;; else
		   (push thing result)))))
    (nreverse result)))

(defun concat-inbetween-text* (things)
  (concat-inbetween-text (flatten things)))

(defun normalize-markup-text (things)
  (let ((things (concat-inbetween-text* things)))
    (if (and (equalp (length things) 1)
	     (stringp (first things)))
	(first things)
	things)))


(defrule eol #\NewLine)

(defrule eof (! (characterp character)))

(defrule blank #\  )

(defrule tab #\Tab)

(defrule spacing (* (or blank tab))
  (:text t))

(defrule spacing* (* (or eol blank tab))
  (:text t))

(defrule letter
    (character-ranges (#\a #\z)
		      (#\A #\Z)
		      (#\0 #\9)))

(defrule word (and
	       (+ (not (or blank tab eol eof)))
	       (& (or blank tab eol eof)))
  (:function (lambda (match)
	       (text (first match)))))

(defrule word* (and
		(+ (not (or blank tab eol eof
			    #\, #\; #\. #\:)))
		(& (or blank tab eol eof
		       #\, #\; #\. #\:)))
  (:function (lambda (match)
	       (text (first match)))))

(defrule list-element (+ (and list-item (? eol)))
  (:function (lambda (match)
	       (make-list-element :items (mapcar #'first match)))))

(defrule list-item (and spacing #\* list-item-text)
  (:function (lambda (match)
	       (make-list-item-element :text (third match)))))

(defrule list-item-text (and (! (or (and eol list-item)
				    (and eol eol)))
			     (? eol)
			     markup-text-line
			     (? list-item-text))
  (:function normalize-markup-text))

(defrule code-element (and #\` #\` code-text #\` #\`)
  (:function (lambda (match)
	       (make-code-element :text (nth 2 match)))))

(defrule code-text (* (not (and #\` #\`)))
  (:text t))

(defrule bold-element (and #\* #\* bold-element-text #\* #\*)
  (:function (lambda (match)
	       (make-bold-element :text (nth 2 match)))))

(defrule bold-element-text (* (not (and #\* #\*)))
  (:text t))

(defrule italic-element (and #\/ #\/ italic-element-text #\/ #\/)
  (:function (lambda (match)
	       (make-italic-element :text (nth 2 match)))))

(defrule italic-element-text (* (not (and #\/ #\/)))
  (:text t))

(defrule markup-element (or code-element
			    bold-element
			    italic-element
			    list-element))

(defrule text-line (+ (and spacing word spacing))
  (:text t))

(defrule text (+ (and spacing* word spacing*))
  (:text t))

(defrule markup-text-line (or (and spacing markup-element spacing markup-text-line)
			      (and spacing word  spacing markup-text-line)
			      (and spacing markup-element)
			      (and spacing word))
  (:function normalize-markup-text))

(defrule markup-text
    (or (and spacing* markup-element spacing* markup-text)
	(and spacing* word spacing* markup-text)
	(and spacing* markup-element)
	(and spacing* word))
  (:function normalize-markup-text))

(defrule docstring-option-name (+ (not (or #\: #\; blank tab eol)))
  (:text t))

(defrule docstring-option-value (+ (not (or #\: #\; blank tab eol)))
  (:text t))

(defrule docstring-option (or (and docstring-option-name spacing #\: spacing docstring-option-value)
			      docstring-option-name)

  (:function (lambda (match)
	       (if (listp match)
		   (make-docstring-option-element :name (first match)
						  :value (nth 4 match))
		   (make-docstring-option-element :name match)))))

(defrule docstring-options (and #\! (+ (and spacing docstring-option spacing #\;)))
  (:function (lambda (match)
	       (make-docstring-options-element :options
					       (mapcar #'second (nth 1 match))))))

(defrule upcase-character (not (or blank tab eol
				   (character-ranges (#\a #\z)))))

(defrule upcase-word (and (+ upcase-character)
			  (& (or tab blank eol eof)))
  (:function (lambda (match)
	       (text (first match)))))

(defrule reference (or upcase-word
		       (and #\` reference-text #\`
			    (? (and
				spacing #\( spacing
				reference-type
				spacing #\)))))
  (:function
   (lambda (match)
     (if (stringp match)
	 (make-ref-element :name match)
	 (let ((name (second match))
	       (type (nth 3 (nth 3 match))))
	   (make-ref-element :name name
			     :type type))))))
  
(defrule reference-text (+ (not #\`))
  (:text t))

(defrule reference-type (+ (not (or tab blank eol #\))))
  (:text t))

(defrule command (and (or #\\ #\@)
		      command-name
		      (? command-options)
		      (? command-args))
  (:function (lambda (match)
	       (destructuring-bind (keyword name options args) match
		 (make-command-element :name name
				       :options options
				       :args args)))))

(defrule command-args (* command-arg))
(defrule command-arg (and #\{ (*(not #\})) #\})
  (:function (lambda (match)
	       (text
		(second match)))))

(defrule command-name (+ (not (or blank tab eol #\` #\{ #\[)))
  (:text t))

(defrule command-options (and #\[ command-options-list #\])
  (:function (lambda (match)
	       (second match))))

(defrule command-options-list (or (and spacing command-option spacing #\,
				       command-options-list)
				  (and spacing command-option spacing))
  (:function (lambda (match)
	       (cons (second match)
		     (nth 4 match)))))

(defrule command-option (and command-option-name
			     spacing
			     (? (and (or #\= #\:)
				     spacing
				     command-option-value)))
  (:function (lambda (match)
	       (cons (first match)
		     (third (third match))))))

(defrule command-option-name
    (and (+ (not (or blank tab eol eof
		     #\, #\; #\. #\: #\= #\])))
	 (& (or blank tab eol eof
		#\, #\; #\. #\: #\=)))
  (:function (lambda (match)
	       (text (first match)))))

(defrule command-option-value
    (and (+ (not (or blank tab eol eof
		     #\, #\; #\. #\: #\= #\]))))
  (:text t))
