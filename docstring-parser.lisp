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

;; Commands
(defstruct (command-element
             (:print-function print-command-element))
  name
  options
  args)

(defun print-command-element (command stream depth)
  (format stream "(:command ~A [~{~A~}]~{{~A}~})"
          (command-element-name command)
          (command-element-options command)
          (command-element-args command)))

;; Specific commands
(defun arg-command-p (command)
  "Checks whether `command` is an **arg** command

   TODO: Check that command options and arguments are valid."
  
  (and (command-element-p command)
       (equalp (command-element-name command) "arg")))

(defstruct (args-element
             (:print-function print-args-element))
  args)

(defun print-args-element (args stream depth)
  (format stream "(:args ~{~A~})" (args-element-args args)))

(defstruct (arg-element
             (:print-function print-arg-element))
  name
  type
  description)

(defun print-arg-element (arg stream depth)
  (format stream "(:arg ~S :type :~A :description ~S)"
          (arg-element-name arg)
          (arg-element-type arg)
          (arg-element-description arg)))

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

(defun normalize-markup-text (things &key (trim (list #\NewLine #\ )))
  (let ((things (concat-inbetween-text* things)))
    (if (and (equalp (length things) 1)
             (stringp (first things)))
        (string-trim trim (first things))
        (let ((beggining (first things))
              (ending (car (last things))))
          (append (list
                   (or (and (stringp beggining)
                            (string-left-trim trim beggining))
                       beggining))
                  (butlast (cdr things))
                  (list
                   (or (and (stringp ending)
                            (string-right-trim trim ending))
                       ending)))))))

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

(defrule word-separator (or blank tab eol eof
                            #\, #\; #\. #\:
                            #\( #\)
                            #\[ #\]
                            #\{ #\}))
(defrule word* (and
                (+ (not word-separator))
                (& word-separator))
  (:function (lambda (match)
               (text (first match)))))

(defrule list-element (+ (and list-item (? eol)))
  (:function (lambda (match)
               (make-list-element :items (mapcar #'first match)))))

(defrule list-item (and spacing (! bold-element) #\* list-item-text)
  (:function (lambda (match)
               (make-list-item-element :text (nth 3 match)))))

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

(defrule markup-element (or top-markup-element
                            sub-markup-element))

(defrule sub-markup-element (or code-element
                                bold-element
                                italic-element
                                reference
                                command))

(defrule top-markup-element (or list-element
                                list-item))

(defrule text-line (+ (and spacing word spacing))
  (:text t))

(defrule text (+ (and spacing* word spacing*))
  (:text t))

(defrule markup-text-line (or (and spacing markup-element spacing sub-markup-text-line)
                              (and spacing markup-element)
                              (and spacing word  spacing sub-markup-text-line)
                              (and spacing word))
  (:function normalize-markup-text))

(defrule sub-markup-text-line (or (and spacing sub-markup-element spacing sub-markup-text-line)
                                  (and spacing sub-markup-element)
                                  (and spacing word  spacing sub-markup-text-line)
                                  (and spacing word)))

(defrule markup-text (* (and markup-text-line (or eol eof)))
  (:function normalize-markup-text))

;; Docstring options

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

;; References

(defrule upcase-character (not (or blank tab eol
                                   (character-ranges (#\a #\z)))))

(defrule upcase-word (and (+ upcase-character)
                          (& (or tab blank eol eof)))
  (:function (lambda (match)
               (text (first match)))))

(defrule reference (and #\` reference-text #\`
                        (? (and
                            spacing #\( spacing
                            reference-type
                            spacing #\))))
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

;; Commands

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
(defrule command-arg (and #\{
			  spacing
			  (* (and (! #\})
				  (or markup-element
				      command-word)
				  spacing))
			  #\})
  (:function (lambda (match)
               (normalize-markup-text
                (third match)))))
(defrule command-word (and
		       (+ (not (or blank tab eol eof #\})))
		       (& (or blank tab eol eof #\})))
  (:function (lambda (match)
               (text (first match)))))

(defrule command-name word*
  (:text t))

(defrule command-options (and #\[ (? command-options-list) #\])
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
    (and (+ (not (or word-separator #\=)))
         (& (or word-separator #\=)))
  (:function (lambda (match)
               (text (first match)))))

(defrule command-option-value
    (and (+ (not (or blank tab eol eof
                     #\, #\; #\. #\: #\= #\]))))
  (:text t))

;; Args

(defrule args-element (or command-args-element
			  textual-args-element))

(defrule textual-args-element (and (or "Args:" "Params:")
				   spacing*
				   (+ (and arg-element spacing*)))
  (:function (lambda (match)
               (make-args-element :args (mapcar #'first (third match))))))

(defrule command-args-element (+ (and (arg-command-p command)
				      spacing*))
  (:function (lambda (match)
	       (flet ((make-arg (command)
			(make-arg-element :name (command-element-name command)
					  :type (car (first (command-element-options command)))
					  :description (first (command-element-args command)))))
		 (make-args-element :args (mapcar (alexandria:compose #'make-arg #'first)
						  match))))))

(defrule arg-element (and "-"
                          spacing
                          arg-name
                          spacing
                          (? (and arg-type spacing))
                          ":"
                          spacing
                          arg-description)
  (:function (lambda (match)
               (make-arg-element :name (third match)
                                 :type (first (nth 4 match))
                                 :description (nth 7 match)))))

(defrule arg-name word*)
(defrule arg-type (and #\( arg-type-name #\))
  (:function (lambda (match)
               (second match))))
(defrule arg-type-name word*)
(defrule arg-description (or (and markup-text-line
                                  eol
                                  (! (and spacing (or arg-element
                                                      returns-element)))
                                  arg-description)
                             markup-text-line)
  (:function (lambda (match)
               (normalize-markup-text match))))

(defstruct (returns-element
             (:print-function print-returns-element))
  returns)

(defun print-returns-element (elem stream depth)
  (format stream "(:returns ~S)" (returns-element-returns elem)))

(defun returns-command-p (command)
  (and (command-element-p command)
       (equalp (command-element-name command) "returns")))

(defrule returns-element (or command-returns-element
			     textual-returns-element))

(defrule command-returns-element (returns-command-p command)
  (:function (lambda (match)
	       (make-returns-element :returns (first (command-element-args match))))))

(defrule textual-returns-element (and "Returns:"
                              spacing
                              return-description)
  (:function (lambda (match)
               (make-returns-element :returns (third match)))))

(defrule return-description (and markup-text-line)
  (:function (lambda (match)
               (normalize-markup-text match))))

(defstruct (docstring-metadata
             (:print-function print-docstring-metadata))
  metadata)

(defun print-docstring-metadata (metadata stream depth)
  (format stream "(:metadata ~{~A~})" (docstring-metadata-metadata metadata)))

(defrule docstring-metadata (+ (and
                                spacing
                                (or docstring-version
                                    docstring-todo
                                    docstring-see
                                    docstring-date
                                    docstring-author
				    docstring-tags)
                                (or eol eof)))
  (:function (lambda (match)
               (make-docstring-metadata :metadata (mapcar #'second match)))))

;; author

(defstruct (docstring-author
             (:print-function print-docstring-author))
  author)

(defun print-docstring-author (author stream depth)
  (format stream "(:author ~S)" (docstring-author-author author)))

(defrule docstring-author (and "Author:" spacing markup-text-line)
  (:function (lambda (match)
               (make-docstring-author :author (third match)))))

;; version

(defstruct (docstring-version
             (:print-function print-docstring-version))
  version)

(defun print-docstring-version (version stream depth)
  (format stream "(:version ~S)" (docstring-version-version version)))

(defrule docstring-version (and "Version:" spacing markup-text-line)
  (:function (lambda (match)
               (make-docstring-version :version (third match)))))

;; date

(defstruct (docstring-date
             (:print-function print-docstring-date))
  date)

(defun print-docstring-date (date stream depth)
  (format stream "(:date ~S)" (docstring-date-date date)))

(defrule docstring-date (and "Date:" spacing markup-text-line)
  (:function (lambda (match)
               (make-docstring-date :date (third match)))))

;; TODO

(defstruct (docstring-todo
             (:print-function print-docstring-todo))
  todo)

(defun print-docstring-todo (todo stream depth)
  (format stream "(:TODO ~S)" (docstring-todo-todo todo)))

(defrule docstring-todo (and "TODO:" spacing markup-text-line)
  (:function (lambda (match)
               (make-docstring-todo :todo (third match)))))

;; See

(defstruct (docstring-see
             (:print-function print-docstring-see))
  references)

(defun print-docstring-see (see stream depth)
  (format stream "(:see ~{~A~})" (docstring-see-references see)))

(defrule docstring-see (and "See:" spacing docstring-see-references)
  (:function (lambda (match)
               (make-docstring-see :references (third match)))))

(defrule docstring-see-references (or (and spacing reference
                                           spacing #\, spacing
                                           docstring-see-references)
                                      (and spacing reference))
  (:function (lambda (match)
               (cons (second match)
                     (nth 5 match)))))

;; Tags

(defstruct (docstring-tags
	     (:print-function print-docstring-tags))
  tags)

(defun print-docstring-tags (tags stream detph)
  (format stream "(:tags ~{~S~^, ~})" (docstring-tags-tags tags)))

(defrule docstring-tags (and "Tags:" spacing tags-list)
  (:function (lambda (match)
	       (make-docstring-tags :tags
				    (third match)))))

(defrule tags-list (or (and spacing docstring-tag
			    spacing #\, spacing
			    tags-list)
		       (and spacing docstring-tag))
  (:function (lambda (match)
	       (cons (second match)
		     (nth 5 match)))))

(defrule docstring-tag word*)

;; Categories

;; Docstrings

;; Function docstring

(defstruct (function-docstring
             (:print-function print-function-docstring))
  options
  short-description
  args
  returns
  long-description
  metadata)

(defun print-function-docstring (docstring stream depth)
  (format stream "(:function-docstring")
  (when (function-docstring-options docstring)
    (format stream " :options ~A" (function-docstring-options docstring)))
  (when (function-docstring-short-description docstring)
    (format stream " :short-description ~S" (function-docstring-short-description docstring)))
  (when (function-docstring-args docstring)
    (format stream " :args ~A" (function-docstring-args docstring)))
  (when (function-docstring-returns docstring)
    (format stream " :returns ~A" (function-docstring-returns docstring)))
  (when (function-docstring-long-description docstring)
    (format stream " :long-description ~S" (function-docstring-long-description docstring)))
  (when (function-docstring-metadata docstring)
    (format stream " :metadata ~A" (function-docstring-metadata docstring)))
  (format stream ")"))

;; Class docstring

(defstruct (class-docstring
             (:print-function print-class-docstring))
  options
  description
  metadata)

(defun print-class-docstring (docstring stream depth)
  (format stream "(:class-docstring")
  (when (class-docstring-options docstring)
    (format stream " :options ~A" (class-docstring-options docstring)))
  (when (class-docstring-description docstring)
    (format stream " :description ~S" (class-docstring-description docstring)))
  (when (class-docstring-metadata docstring)
    (format stream " :metadata ~A" (class-docstring-metadata docstring)))
  (format stream ")"))

(defrule class-docstring
    (and (? docstring-options)
         (? (and spacing* docstring-long-description))
         (? (and spacing* docstring-metadata))
         spacing*)
  (:function (lambda (match)
               (destructuring-bind (options
                                    description
                                    metadata
                                    spacing) match
                 (make-class-docstring
                  :options options
                  :description (second description)
                  :metadata (second metadata))))))

;; Package docstring

(defstruct (package-docstring
             (:print-function print-package-docstring))
  options
  description
  metadata)

(defun print-package-docstring (docstring stream depth)
  (format stream "(:package-docstring")
  (when (package-docstring-options docstring)
    (format stream " :options ~A" (package-docstring-options docstring)))
  (when (package-docstring-description docstring)
    (format stream " :description ~S" (package-docstring-description docstring)))
  (when (package-docstring-metadata docstring)
    (format stream " :metadata ~A" (package-docstring-metadata docstring)))
  (format stream ")"))

(defrule package-docstring
    (and (? docstring-options)
         (? (and spacing* docstring-long-description))
         (? (and spacing* docstring-metadata))
         spacing*)
  (:function (lambda (match)
               (destructuring-bind (options
                                    description
                                    metadata
                                    spacing) match
                 (make-package-docstring
                  :options options
                  :description (second description)
                  :metadata (second metadata))))))


;; Docstring parsing

(defrule docstring-element (or args-element
                               returns-element
                               docstring-metadata))

(defrule function-docstring
    (and (? docstring-options) spacing* docstring-short-description
         (? (and spacing* args-element))
         (? (and spacing* returns-element))
         (? (and spacing* docstring-long-description))
         (? (and spacing* docstring-metadata))
         spacing*)
  (:function (lambda (match)
               (destructuring-bind (options sp1 short-description
                                            args
                                            returns
                                            long-description
                                            metadata
                                            spacing) match
                 (make-function-docstring
                  :options options
                  :short-description short-description
                  :args (second args)
                  :returns (second returns)
                  :long-description (second long-description)
                  :metadata (second metadata))))))

(defrule docstring-short-description (and (! (or (and spacing docstring-element)
                                                 (and eol eol)))
                                          markup-text-line
                                          (? (and eol
                                                  docstring-short-description)))
  (:function (lambda (match)
               (normalize-markup-text (list (second match)
                                            (third match))))))

(defrule docstring-long-description (and (! (and spacing docstring-element))
                                         markup-text-line
                                         (? (and spacing*
                                                 docstring-long-description)))

  (:function (lambda (match)
               (normalize-markup-text (list (second match)
                                            (third match))))))

;; Api

(defun parse-function-docstring (docstring)
  (parse 'function-docstring docstring))

(defun parse-class-docstring (docstring)
  (parse 'class-docstring docstring))

(defun parse-class-slot-docstring (docstring)
  (parse 'markup-text docstring))

(defun parse-package-docstring (docstring)
  (parse 'package-docstring docstring))
