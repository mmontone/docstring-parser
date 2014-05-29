;;;; docstring-parser.lisp

(in-package #:docstring-parser)

(defrule eol #\NewLine)

(defrule blank #\  )

(defrule tab #\Tab)

(defrule spacing (* (or blank tab))
  (:text t))

(defrule spacing* (* (or eol spacing)))

(defrule letter
    (character-ranges (#\a #\z)
		      (#\A #\Z)
		      (#\0 #\9)))

(defrule word (+ (not (or blank tab eol)))
  (:text t))

(defrule list-element (+ list-item))
(defrule list-item (and #\* list-item-text)
  (:function (lambda (match)
	       (list :li (second match)))))

(defrule list-item-text markup-text-line)

(defrule code-element (and #\` #\` code-text #\` #\`)
  (:function (lambda (match)
	       (list :code (nth 2 match)))))
(defrule code-text (* (not (and #\` #\`))) (:text t))

(defrule markup-element (or list-element
			    code-element))

(defrule text (+ (and spacing word spacing)) (:text t))
(defrule markup-text (or (and spacing markup-element spacing markup-text-line)
			 (and spacing word  spacing markup-text-line)
			 (and spacing markup-element)
			 (and spacing word))
  (:function normalize-markup-text))

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
