(in-package #:docstring-parser)

(defstruct (list-element
	     (:print-function print-list-element))
  items)

(defun print-list-element (elem stream depth)
  (format stream "~{~A~}" (list-element-items elem)))

(defstruct (list-item-element
	     (:print-function print-list-item-element))
  text)

(defun print-list-item-element (elem stream depth)
  (format stream "* ~A" (list-item-element-text elem)))

(defstruct (italic-element
	     (:print-function print-italic-element))
  text)

(defun print-italic-element (elem stream depth)
  (format stream "//~A//" (italic-element-text elem)))

(defstruct (bold-element
	     (:print-function print-bold-element))
  text)

(defun print-bold-element (elem stream depth)
  (format stream "**~A**" (bold-element-text elem)))

(defstruct (code-element
	     (:print-function print-code-element))
  text)

(defun print-code-element (elem stream depth)
  (format stream "``~A``" (code-element-text elem)))

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

(defrule word (+ (not (or blank tab eol)))
  (:text t))

(defrule list-element (and eol (+ (and list-item (? eol))))
  (:function (lambda (match)
	       (make-list-element :items (mapcar #'first (second match))))))

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

(defrule markup-element (or list-element
			    code-element
			    bold-element
			    italic-element))

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
    (or (and spacing* markup-element spacing* markup-text-line)
	(and spacing* word  spacing* markup-text-line)
	(and spacing* markup-element)
	(and spacing* word))
  (:function normalize-markup-text))
