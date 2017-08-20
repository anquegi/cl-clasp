;;;; -*- Mode:Common-Lisp; Package:CLASP-INTERFACE; Base:10 -*-
;;;; *-* File: Titanic: /usr/users/eksl/group-files/clasp/v3dev/interface/data-manipulation-commands.lisp *-*
;;;; *-* Last-edit: Thursday, February 18, 1993  00:54:05; Edited-By: carlson *-* 
;;;; *-* Machine: Miles (Explorer II, Microcode 489) *-*
;;;; *-* Software: TI Common Lisp System 6.49 *-*
;;;; *-* Lisp: TI Common Lisp System 6.49  *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *                                                                        *
;;;; *                       Data Manipulation Commands                       *
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
;;; Copyright (c) 1990 - 1993 University of Massachusetts
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
;;;  02-18-93 File Created.  (carlson)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;; --*--

(in-package :CLASP-INTERFACE)

;;; --*--
;;; ***************************************************************************


(define-command-table clasp-data-manipulation)

(add-menu-item-to-command-table 'clasp "Manipulate" 
                                :menu 'clasp-data-manipulation 
				:documentation "Data manipulation"
                                :errorp nil :after "Describe")

(define-clasp-command (com-rename-dataset
		       :command-table clasp-data-manipulation :name t
		       :menu ("Rename Dataset"
			      :documentation
			      "Change the name of a dataset"))
    ((dataset-1 'dataset :prompt "Dataset")
     (string-2 'string :prompt "New Name"))
  (rename-dataset dataset-1 string-2)
  (terpri)
  (update-datasets-display))

#+ignore
(defclaspcom rename-dataset data-manipulation 
  (dataset string) (t t) 
  () ()
  :input-options ((:prompt "Dataset") (:prompt "New Name")))

(define-clasp-command (com-rename-variable
		       :command-table clasp-data-manipulation :name t
		       :menu ("Rename Variable"
			      :documentation
			      "Change the name of a variable"))
    ((column-1 'column :prompt "Variable")
     (string-2 'string :prompt "New Name"))
  (rename-variable column-1 string-2)
  (update-datasets-display))

#+ignore
(defclaspcom rename-variable data-manipulation
  (column string) (t t) 
  () ()
  :input-options ((:prompt "Variable") (:prompt "New Name")))

(define-clasp-command (com-delete-result
		       :command-table clasp-data-manipulation :name t
		       :menu ("Delete result"
			      :documentation "Delete something"))
    ((result-1 'icon :prompt "Delete what?" :mapping-option :map))
  (delete-result result-1))

(defmethod delete-result ((self result))
  (close-it self)
  (clasp::kill self)
  (update-results-display))

(defmethod delete-result ((self dataset))
  (close-it self)
  (delete-dataset self)
  (update-datasets-display))

(defmethod delete-result ((self column))
  (close-it self)
  (delete-variable self)
  (update-datasets-display))

#+ignore
(define-clasp-command (com-delete-dataset
		       :command-table clasp-data-manipulation :name t
		       :menu ("Delete Dataset"
			      :documentation "Delete a dataset"))
    ((dataset-1 'dataset :prompt "Dataset"))
  (delete-dataset dataset-1)
  (update-datasets-display))

#+ignore
(defclaspcom delete-dataset data-manipulation
  (dataset) (t) 
  () ()
  :input-options ((:prompt "Dataset")))

#+ignore
(define-clasp-command (com-delete-variable
		       :command-table clasp-data-manipulation :name t
		       :menu ("Delete Variable"
			      :documentation "Delete a variable"))
    ((variable-1 'variable :prompt "Variable"))
  (delete-variable variable-1)
  (update-datasets-display))

#+ignore
(defclaspcom delete-variable data-manipulation
  (variable) (t) 
  () ()
  :input-options ((:prompt "Variable")))

(define-clasp-command (com-make-dataset-from-rows
		       :command-table clasp-data-manipulation :name t
		       :menu ("Make Dataset from Rows"
			      :documentation
			      "Create a dataset from row-major data"))
    ((name 'string :prompt "Name")
     (data '(sequence (sequence (or number symbol))) :prompt "Data")
     (variable-names '(sequence string) :prompt "Variable names"))
  (let ((new-dataset (make-dataset-from-rows name data variable-names)))
    (present-with-update new-dataset
			 :stream *standard-output* :view +dialog-view+)))

(define-clasp-command (com-make-dataset-from-columns
		       :command-table clasp-data-manipulation :name t
		       :menu ("Make Dataset from Columns"
			      :documentation
			      "Create a dataset from column-major data"))
    ((name 'string :prompt "Name")
     (data '(sequence (sequence (or number symbol))) :prompt "Data")
     (variable-names '(sequence string) :prompt "Variable names"))
  (let ((new-dataset (make-dataset-from-columns name data variable-names)))
    (present-with-update new-dataset
			 :stream *standard-output* :view +dialog-view+)))

(define-clasp-command (com-add-variable-to-dataset
		       :command-table clasp-data-manipulation :name t
		       :menu ("Add Variable To Dataset"
			      :documentation
			      "Add a new variable to a dataset"))
    ((dataset-1 'dataset :prompt "Dataset")
     (expression-2 'expression :prompt "Data")
     (string-3 'string :prompt "Name"))
  (let ((values
	 (multiple-value-list (funcall #'add-variable-to-dataset
				       dataset-1
				       expression-2
				       string-3))))
    (assert (= (length values) 1) nil
      "`~a' returned ~s values, we were expecting ~s"
      'add-variable-to-dataset (length values) 1)
    (present-with-update (nth 0 values)
			 :stream *standard-output* :view +dialog-view+)))

#+ignore
(defclaspcom add-variable-to-dataset data-manipulation
  (dataset expression string) (t t t)
  (t) (column)
  :input-options ((:prompt "Dataset") (:prompt "Data") (:prompt "Name")))

(define-clasp-command (com-partition-dataset
		       :command-table clasp-data-manipulation :name t
		       :menu ("Partition Dataset"
			      :documentation
			      "Select rows from a dataset"))
    ((dataset-1 'dataset :prompt "Dataset")
     (expression-2 'partition-clause :prompt "Partition Clause")
     (variables '(sequence column) :prompt "Includes variables [default All]"
		:default (dataset-variables dataset-1) :display-default nil))
  (let ((values
	 (multiple-value-list (funcall
			       #'partition-dataset
			       dataset-1
			       expression-2
			       variables))))
    (assert (= (length values) 1) nil
      "`~a' returned ~s values, we were expecting ~s"
      '"partition dataset" (length values) 1)
    (present-with-update (nth 0 values)
			 :stream *standard-output* :view +dialog-view+)))

#+ignore
(defclaspcom ("Partition Dataset" :function partition-dataset)
    data-manipulation
  (dataset expression) (t t) 
  (t) ((or dataset (sequence dataset)))
  :input-options ((:prompt "Dataset") (:prompt "Partition Clause")))

(define-clasp-command (com-partition-on
		       :command-table clasp-data-manipulation :name t
		       :menu ("Partition On"
			      :documentation
			      "Partition a dataset on one of its variables"))
    ((dataset 'dataset :prompt "Dataset")
     (variable 'column :prompt "Partition variable")
     (variables '(sequence column) :prompt "Included variables [default All]"
		:default (dataset-variables dataset) :display-default nil))
  (setf variables (remove-duplicates (cons variable variables)))
  (let ((value (partition-dataset dataset `(.on. ,(name variable)) variables)))
    (present-with-update value :stream *standard-output* :view +dialog-view+)))
  
(define-clasp-command (com-merge-datasets
		       :command-table clasp-data-manipulation
		       :name t
		       :menu ("Merge Datasets"
			      :documentation
			      "Combine datasets, stacking like-named columns"))
    ((datasets '(sequence dataset) :prompt "Datasets"))
  (let ((new-dataset (merge-datasets datasets)))
    (present-with-update new-dataset
			 :stream *standard-output* :view +dialog-view+)))
   
(define-clasp-command (com-open :command-table clasp-data-manipulation :name t
				:menu ("Open"
				       :documentation
				       "Display a result in a window"))
    ((icon 'icon :prompt "Open what?" :mapping-option :map))
  (open-it icon :refresh t))

(define-clasp-command (com-close :command-table clasp-data-manipulation :name t
				 :menu ("Close"
					:documentation
					"Remove a window displaying a result"))
    ((icon 'icon :prompt "Close what?" :mapping-option :map))
  (close-it icon))

#+ignore
(clim:define-presentation-to-command-translator open-icon-command
    (dragged-icon com-open icon-command-table
		  :gesture :select
		  :pointer-documentation
		  ((object presentation context-type frame stream)
		   (declare (ignore presentation frame context-type))
		   (format stream "Open ~a" (clasp::name object))
		   )
		  :documentation
		  ((object presentation context-type frame stream)
		   (declare (ignore presentation frame context-type))
		   (format stream "Open ~a" (clasp::name object))
		   )
		  :menu t
		  :tester
		  ((object)
		   (null (display-window object))))
  (object)
  (list object)
  )

(clim:define-presentation-to-command-translator open-result
    (result com-open clasp-data-manipulation
	    :gesture :select
	    :pointer-documentation
	    ((object presentation context-type frame stream)
	     (declare (ignore presentation frame context-type))
	     (format stream "Open ~a" (clasp::name object)))
	    :documentation
	    ((object presentation context-type frame stream)
	     (declare (ignore presentation frame context-type))
	     (format stream "Open ~a" (clasp::name object)) )
	    :menu t
	    :tester
	    ((object)
	     (and (null (display-window object))
		  (clasp::find-instance-by-name (name object) (class-of object)
						t))))
  (object)
  (list (list object)))

(clim:define-presentation-to-command-translator close-result
    (result com-close clasp-data-manipulation
	    :gesture :select
	    :pointer-documentation
	    ((object presentation context-type frame stream)
	     (declare (ignore presentation frame context-type))
	     (format stream "Close ~a" (clasp::name object)))
	    :documentation
	    ((object presentation context-type frame stream)
	     (declare (ignore presentation frame context-type))
	     (format stream "Close ~a" (clasp::name object)))
	    :menu t
	    :tester
	    ((object)
	     (and (not (null (display-window object)))
		  (clasp::find-instance-by-name (name object) (class-of object)
						t))))
  (object)
  (list (list object)))

(define-clasp-command (com-describe :command-table clasp-data-manipulation
				    :name t
				    :menu ("Describe"
					   :documentation
					   "Show the description of a result"))
    ((result 'result :prompt "Result"))
  (present (description result) 'string
	   :stream *standard-output* :view +dialog-view+)
  (terpri))

(define-presentation-to-command-translator describe-result
    (result com-describe clasp-data-manipulation
	    :gesture :describe
	    :pointer-documentation
	    ((object presentation context-type frame stream)
	     (declare (ignore presentation frame context-type))
	     (format stream "Describe ~a" (clasp::name object)))
	    :documentation
	    ((object presentation context-type frame stream)
	     (declare (ignore presentation frame context-type))
	     (format stream "Describe ~a" (clasp::name object)))
	    :menu t)
  (object)
  (list object))

;;; ***************************************************************************
;;; EOF




