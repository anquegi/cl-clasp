;;;; -*- Mode:Common-Lisp; Package:CLASP; Base:10 -*-
;;;; *-* File: Titanic: /usr/users/eksl/group-files/clasp/clasp/development/manipulation-fns.lisp *-*
;;;; *-* Last-edit: Friday, March 19, 1993  19:06:12; Edited-By: Anderson *-* 
;;;; *-* Machine: Dizzy (Explorer II, Microcode 489) *-*
;;;; *-* Software: TI Common Lisp System 6.49 *-*
;;;; *-* Lisp: TI Common Lisp System 6.49  *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *                                                                        *
;;;; *                       Data Manipulation Functions                      *
;;;; *                                                                        *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Adam Carlson
;;;             Experimental Knowledge Systems Laboratory
;;;             Paul R. Cohen, Principal Investigator
;;;             David L. Westbrook, Systems Manager
;;;             David M. Hart, Laboratory Manager
;;;             Department of Computer Science
;;;             University of Massachusetts
;;;             Amherst, Massachusetts 01003.
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;; Copyright (c) 1990 - 1994 University of Massachusetts
;;; Department of Computer and Information Science
;;; Experimental Knowledge Systems Laboratory
;;; Professor Paul Cohen, Director.
;;; All rights reserved.
;;; 
;;; Permission to use, copy, modify and distribute this software and its
;;; documentation is hereby granted without fee for non-commercial uses only 
;;; (not for resale), provided that the above copyright notice of EKSL, this 
;;; paragraph and the one following appear in all copies and in supporting 
;;; documentation.
;;; EKSL makes no representation about the suitability of this software for any
;;; purposes.  It is provided "AS IS", without express or implied warranties
;;; including (but not limited to) all implied warranties of merchantability
;;; and fitness for a particular purpose, and notwithstanding any other
;;; provision contained herein.  In no event shall EKSL be liable for any
;;; special, indirect or consequential damages whatsoever resulting from loss
;;; of use, data or profits, whether in an action of contract, negligence or
;;; other tortuous action, arising out of or in connection with the use or
;;; performance of this software, even if EKSL is
;;; advised of the possiblity of such damages.
;;; 
;;; For more information write to clasp-support@cs.umass.edu
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  02-25-93 File Created.  (carlson)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;; --*--

(in-package :clasp)

;;; --*--
;;; ***************************************************************************


(defun create-new-column-from-function (dataset expression)
   "`expression' must be a valid lisp expression, it may contain the
names of variables in `dataset'.  The rows of `dataset' are mapped
over, substituting the values of the variables in `expression', after
substitution, `expression' is evaluated.  This produces a new column
of data which is added as a variable to `dataset'."
  (check-type dataset dataset)
  (check-type expression list)
  
  (let ((data (apply-function-to-variables dataset expression)))
    (add-variable-to-dataset dataset data (princ-to-string expression))))

(defun apply-function-to-variables (dataset expression)
  (let ((variable-list (get-variable-list expression
					  (dataset-variables dataset)))
	data-list
	lambda-expression)
    (setf data-list (mapcar #'(lambda (x)
				(variable-value
				 (find x (dataset-variables dataset)
				       :key #'id)))
			    variable-list))
    (setf lambda-expression
      (compile nil (list 'lambda variable-list expression)
	       #+lucid :optimize-message #+lucid nil
	       ))
    (apply #'mapcar lambda-expression data-list)))

(defun recode (value old-values new-values)
  "Maps `value' from it's position in `original-values' to the
value at the equivalent position in `new-values'.  For instance,
(recode 2 '(1 2 3) '(a b c)) would return 'b.  This function can be used
with create-new-column-from-function to recode a categorical variable.
From the program interface, it is recommended that ``recode-list''
be used instead."
  (check-type value atom)
  (check-type old-values list)
  (check-type new-values list)

  (nth (position value old-values) new-values))

(defun recode-list (list old-values new-values)
  "Maps each element of `list' from it's position in `old-values' to
the value at the equivalent position in `new-values'."
  (map nil
       #'(lambda (new old)
	   (setf list (substitute new old list)))
       new-values
       old-values)
  list)

(defun get-variable-list (expression legal-variables)
  (unless (null expression)
    (let (variable-list)
      (mapcar
	#'(lambda (x)
	    (cond
	      ((listp x)
	       (mapcar #'(lambda (x)
			   (pushnew x variable-list))
		       (get-variable-list x legal-variables)))
	      ((find x legal-variables :key #'id)
	       (pushnew x variable-list))
	      ((and (or (symbolp x) (stringp x))
		    (find x legal-variables :key #'name :test #'string-equal))
	       (pushnew x variable-list))))
	expression)
      (remove-duplicates variable-list))))

;;; ***************************************************************************
;;;; Partitioning
;;; ***************************************************************************

;;; Note: the partitioning code is going to have to make actual copies
;;; of the variable objects in order to make proper use of the
;;; variable-value method.  As it is there is no way for variable value
;;; to know whether a variable belongs to a "source" dataset or a
;;; partitioned view of a source dataset, therefore it always gives all
;;; the data.

  "Often it is desirable to operate on a portion of a dataset, or to
separate out different parts of a dataset according to the values of
some of the variables.  When a dataset is broken up this way, it is
called partitioning, and the parts of the dataset created are called
partitions.  partition-dataset takes a dataset and a partition-clause
which describes how to split the dataset up, and returns a list of
datasets, each one containing one partition.  (Note, this does not
destroy the original dataset.)

Partition clauses are made up of boolean operators, and the
partitioning operator, ``.on.''.  The boolean operators act as
expected and are explained in detail in the reference manual.  The
.on. operator takes any number of variables as its operands, and
separates the dataset into different partitions for each unique set of
values of its operands.  For example, if a dataset consisted of a
boolean variable ``key1'', a categorical variable ``key2'', with
values 'a, 'b and 'c, and a data variable ``y'', and was partitioned
with a partitioning clause of (.on. key1 key2), then at most six new
datasets would be created, one containing all the rows of the original
dataset where key1 = true and key2 = 'a, one with all the rows where
key1 = false and key2 = 'a, one with all the rows where key1 = true
and key2 = 'b, and so on.  If a partition would be empty (i.e. if
there were no rows where key1 = false and key2 = 'c), then no dataset
is created for that partition, which is why AT MOST six new datasets
would be created in the above example.

The .on. operator can be used in conjunction with the boolean
operators.  When this happens, the .on. operator can be thought of as
a macro which causes a set of partitioning clauses to be created, each
exactly like the original, except that .on. is replaces with an .==.
operator and one of the unique values of the key.  If there are
multiple keys in the .on., it is replaced by (.and. (.==. key1 value11)
(.==. key2 value21), etc.).  In the above example, a partition clause
of (.and. (.==. key1 true) (.on. key2)) would cause up to three
datasets to be created.  Each would contain rows where key1 was true,
and one would be created for key2 = 'a, one for key2 = 'b and one for
key2 = 'c."

(defun partition-dataset (old-dataset partition-clause &optional variables
			  &key (unique-variable-names-p t))
  (setf old-dataset (get-dataset old-dataset))
  (if variables
      (setf variables (mapcar #'(lambda (x) (get-variable x old-dataset))
			      variables))
    (setf variables (dataset-variables old-dataset)))
  (setf partition-clause (replace-on-clauses partition-clause old-dataset))
  (if (listp (car partition-clause))
      (let ((counter 0))
	(mapcar #'(lambda (x)
		    (make-dataset-from-rtm-boolean
		     old-dataset (replace-variables-with-attributes x) variables
		     :new-dataset-name (infix x)
		     :variable-postfix (and unique-variable-names-p
					    (incf counter))))
		partition-clause))
    (make-dataset-from-rtm-boolean old-dataset partition-clause variables
				   :new-dataset-name (infix partition-clause))
      ))

(defun replace-variables-with-attributes (where-clause)
  (cond
   ((null where-clause)
    nil)
   ((listp where-clause)
    (mapcar #'replace-variables-with-attributes where-clause))
   ((typep where-clause 'clasp::variable)
    (car (clasp::variable-attribute where-clause)))
   (t where-clause)))

(defconstant *where-clause-infix-translators*
    '((.and. . and) (.or. . or) (.not. . not)
      (.==. . =) (.<=. . <=) (.>=. . >=) (./=. . /=) (.>>. . >) (.<<. . <)
      (.=><=. => <=) (.=><<. => <) (.>><=. > <=) (.>><<. > <)
      (.<==>. <= =>) (.<=>>. <= >) (.<<=>. < =>) (.<<>>. < >)
      (.min. . min) (.max. . max)
      (.pred. . pred) (.succ. . succ) (.floor. . floor) (.ceiling. . ceiling)))

(defun infix (where-clause)
  (cond
   ((member (car where-clause) '(.and. .or. .not.))
    (format nil "~a~{-~a~}"
	    (cdr (assoc (first where-clause) *where-clause-infix-translators*))
	    (mapcar #'infix (cdr where-clause))))
   ((member (car where-clause) '(.==. .<=. .>=. ./=. .>>. .<<.))
    (format nil "~a-~a-~a"
	    (name (second where-clause))
	    (cdr (assoc (first where-clause) *where-clause-infix-translators*))
	    (third where-clause)))
   ((member (car where-clause) '(.=><=. .=><<. .>><=. .>><<.
				 .<==>. .<=>>. .<<=>. .<<>>.))
    (format nil "~a-~a-~a-~a-~a"
	    (third where-clause)
	    (car (cdr (assoc (first where-clause)
			     *where-clause-infix-translators*)))
	    (name (second where-clause))
	    (cadr (cdr (assoc (first where-clause)
			      *where-clause-infix-translators*)))
	    (fourth where-clause)))
   ((member (car where-clause) '(.min. .max.))
    (format nil "~a-~a"
	    (cdr (assoc (first where-clause) *where-clause-infix-translators*))
	    (name (second where-clause))))
   ((member (car where-clause) '(.pred. .succ. .floor. .ceiling.))
    (format nil "~a-~a-~a"
	    (cdr (assoc (first where-clause) *where-clause-infix-translators*))
	    (name (second where-clause))
	    (third where-clause)))))
   

(defun replace-on-clauses (partition-clause dataset)
  (let* (partitioning-keys
	 partitioning-values
	 (temp-partition-clause (copy-list partition-clause))
	 (new-partition-clauses (list (copy-list partition-clause))))
    (when (eq (car temp-partition-clause) '.on.)
      (setf partitioning-keys (cdr temp-partition-clause))
      (setf partitioning-values (get-distinct-values partitioning-keys dataset))
      (setf new-partition-clauses
	(mapcan #'(lambda (partition-clause)
		    (replace-on-clause partitioning-values partition-clause dataset temp-partition-clause))
		new-partition-clauses))
      (return-from replace-on-clauses new-partition-clauses))
    (do ((on-clause (find-on-clause temp-partition-clause)
		    (find-on-clause temp-partition-clause)))
	((null on-clause) new-partition-clauses)
      (setf partitioning-keys (cdr on-clause))
      (setf partitioning-values (get-distinct-values partitioning-keys dataset))
      (setf new-partition-clauses
	    (mapcan #'(lambda (partition-clause)
		        (replace-on-clause partitioning-values partition-clause dataset on-clause))
		    new-partition-clauses))
      (setf temp-partition-clause
	    (car (replace-on-clause partitioning-values temp-partition-clause dataset on-clause))))))

(defun replace-on-clause (key-values partition-clause dataset
			  &optional (on-clause
				     (find-on-clause partition-clause)))
  (let (partition-clauses
	(replace-counter 0))
    (dolist (key-value key-values)
      (setf replace-counter 0)
      (push
	(subst-if
	  (make-new-on-clause on-clause key-value dataset)
	  #'(lambda (x)
	      (cond
		((or (> replace-counter 0)
		     (atom x))
		 nil)
		((eq (car x) '.on.)
		 (incf replace-counter)
		 t)
		(t nil)))
	  partition-clause)
	partition-clauses))
    partition-clauses))

(defun make-new-on-clause (on-clause key-value dataset)
  (let ((variables (mapcar #'(lambda (x) (get-variable x dataset)) (cdr on-clause))))
    (if (= (length variables) 1)
	(list '.==. (car variables) (car key-value))
	(cons '.and. (mapcar #'(lambda (key-name key-value)
				 (list '.==. key-name key-value))
			     variables key-value)))))

(defun find-on-clause (partition-clause)
  (cond
    ((null partition-clause) nil)
    ((eq '.on. (car partition-clause))
     partition-clause)
    ((member (car partition-clause) '(.and. .or. .not.))
     (do ((temp)
	  (sub-clause (cdr partition-clause) (cdr sub-clause)))
	 ((or (null sub-clause)
	      (setf temp (find-on-clause (car sub-clause))))
	  temp)))
    (t nil)))

(defun get-partitioning-keys (partition-clause)
  (cond
    ((null partition-clause) (values))
    ((eq '.on. (car partition-clause))
     (cdr partition-clause))
    ((member (car partition-clause) '(.and. .or. .not))
     (remove-duplicates (apply #'append
			       (mapcar #'get-partitioning-keys
				       (cdr partition-clause)))))
    (t (values))))

(defun get-distinct-values (key dataset)
  (let* ((variables (mapcar #'(lambda (x) (get-variable x dataset)) key))
	 (distinct-values-hash (make-hash-table :test #'equal))
	 (all-rows (dataset-to-rows dataset :columns variables))
	 distinct-values)
    (dolist (value all-rows)
	    (incf (gethash value distinct-values-hash 0)))
    (maphash #'(lambda (key value) (declare (ignore value))
		 (push key distinct-values)) distinct-values-hash)
    distinct-values))

#+ignore
(defun nth-elt-of-cross-product (n &rest sets)
  "Suppose we created a multidimensional array, with as many dimensions as
we have sets, and in which the size of each dimension were equal to the size of
the corresponding set.  That array would have exactly as many elements as the
cross product of all these sets, and we could easily put the elements of the
cross product into the array by stepping the index for each dimension through
each of the sets.  But this array notion also gives a unique index to every
element of the cross product.  This function works as if we created such an
array and then did an `array-row-major-aref' into it."
  ;; The code looks just like `decode-row-major-index'
  (let ((remainder (mod n (reduce #'* sets :key #'length)))
	index-in-this-dimension)
    (utils:with-collection
      (dotimes (d (- (length sets) 1))
	(multiple-value-setq (index-in-this-dimension remainder)
	  (truncate remainder (reduce #'* sets :start (1+ d)
					 :key #'length)))
	(utils:collect (elt (elt sets d) index-in-this-dimension)))
      (utils:collect (elt (elt sets (- (length sets) 1)) remainder)))))

(defun make-dataset-from-rtm-boolean (old-dataset where &optional variables
				      &key new-dataset-name variable-postfix)
  (let ((new-dataset (make-dataset
		      :name (or new-dataset-name
				(format nil "~a where ~a"
					(name old-dataset) where))
		      :description (format nil "~a where ~a"
					   (description old-dataset) where)))
	new-variable
	(row 0))
    (dolist (variable (get-variable-list where
					 (dataset-variables old-dataset)))
      (setf new-variable (find variable (dataset-variables old-dataset)
			       :key #'id))
      (unless (rtm-index-p (dataset-write-data old-dataset)
			   (car (variable-attribute new-variable)))
	(create-index (dataset-write-data old-dataset)
		      (car (variable-attribute new-variable)))))
    (setf (dataset-read-data new-dataset)
      (make-clasp-symbol "VIEW" :rtm-table))
    (setf (dataset-write-data new-dataset) (dataset-write-data old-dataset))
    (if (dataset-where-clause old-dataset)
	(setf (dataset-where-clause new-dataset)
	  (list '.and.
		(dataset-where-clause old-dataset)
		(process-where-clause where)))
      (setf (dataset-where-clause new-dataset)
	(process-where-clause where)))
    (create-view (dataset-read-data new-dataset)
		 :from (dataset-write-data new-dataset)
		 :where (dataset-where-clause new-dataset))
    (dolist (old-variable variables)
      (unless (eq (id old-variable) 'row-number)
	(setf new-variable
	  (make-instance *variable-class*
	    :name (make-clasp-symbol
		   (format nil "~a~@[-~a~]" (name old-variable)
			   variable-postfix)
		   :variable new-dataset)
	    :id (id old-variable)
	    :attribute (variable-attribute old-variable)
	    :dataset new-dataset
	    :source (id new-dataset)))
	(push-end new-variable (dataset-variables new-dataset))))
    (setf new-variable
      (make-variable :name "ROW-NUMBER"
		     :attribute (list (make-clasp-symbol
				       (concatenate 'string
					 (string (name new-dataset))
					 " ROW-NUMBER") :data)
				      :number)
		     :dataset new-dataset
		     :rtm-table (dataset-write-data new-dataset)))
    (push new-variable (dataset-variables new-dataset))
    (do-selection  (:from (dataset-write-data new-dataset)
			  :where (dataset-where-clause new-dataset)
			  :order-by 'row-number)
      (incf row)
      (setf (attr-value (car (variable-attribute new-variable))) row))
    (setf (dataset-rows new-dataset) row)
    new-dataset))

;;; ***************************************************************************
;;; EOF
