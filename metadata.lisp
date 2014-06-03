(defpackage metadata
  (:use #:cl)
  (:documentation "Metadata for Common Lisp structures"))

(in-package :metadata)

;; Idea

(defun* (make-person
	 ((fullname string "The person fullname")
	  (age integer "The person age"))
	 (:short-description "Creates a person using " (:arg "fullname")
			     " and " (:arg "age"))
	 (:long-description "Creates a person using " (:arg "fullname")
			    " and " (:arg "age"))
	 (:returns "A " (:ref (:type 'class) 'person))
	 (:author "Mariano Montone")
	 (:tags :creation :person)
	 (:categories :instance-creation :model)
	 (:todo "Test it"))
    
    (make-instance 'person :fullname fullname
		   :age age))

(defclass* person ()
  ((fullname :type string
	     :documentation "The person fullname")
   (age :type integer
	:documentation "The person age"))
  (:documentation "A person")
  (:author "Mariano Montone")
  (:tags :model :person)
  (:categories :model)
  (:see (:ref (:type 'class) 'user)))

;; Implementation

;; (setf (get '<structure-name> :metadata) <metadata>)

;; We could even store the source code (the sexp tree) in the metadata
