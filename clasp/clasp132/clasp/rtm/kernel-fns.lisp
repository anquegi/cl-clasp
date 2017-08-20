;;; -*- Mode:Common-Lisp; Base:10 -*-
;;;; *-* Last-edit: Tuesday, July 28, 1992  14:44:13; Edited-By: LOISELLE *-* 

;;; Version 2.2, July 1992.
;;; ***************************************************************************
;;;
;;; Written by: Paul Silvey
;;;             Department of Computer and Information Science
;;;             University of Massachusetts
;;;             Amherst, MA 01003
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
;;; -- * --

#+OLD
(cl:defpackage "RTM"
  (:use "COMMON-LISP"))

(in-package "RTM")

#+EXPLORER
(import '(ticl:push-end))

(export '(domain-encode-and-verify domain-decode table-changed-p
          domain-name domain-numeric-p
          rtm-tables rtm-views rtm-samples
	  #-EXPLORER push-end
	  ))

;;; -- * --
;;;
;;; Utility functions and Macros:
;;;

#-EXPLORER
(defmacro push-end (item item-list)
  "This is similar to PUSH except that it puts the new element at the end
of the existing list.  This preserves the order of the elements as they are added to
the list."
  `(setf ,item-list (nconc ,item-list (cons ,item nil))))

;;;
;;; User Message Interface Macros:
;;;

(defun rtm-error (msg1 &rest msgs)
  (format *rtm-error-stream* "~&RTM Error: ~a~{~a~}~%" msg1 msgs)
  (error "~&RTM Error: ~a~{~a~}~%" msg1 msgs))

(defun rtm-warning (msg1 &rest msgs)
  (format *rtm-warning-stream* "~&RTM Warning: ~a~{~a~}~%" msg1 msgs))

(defun rtm-notification (msg1 &rest msgs)
  (format *rtm-notification-stream* "~&RTM Notification: ~a~{~a~}~%"
	  msg1 msgs))

;;;
;;; Micellaneous Constants, Parameters, and Variables:
;;;

(defconstant *rtm-table-expansion-factor* 1.5
  "Expansion factor for automatic table size adjustment.")

(defparameter *rtm-initial-nn-priority-queue-size* 100)
(defparameter *rtm-initial-table-size* 100
  "Default initial table size.")

(defvar *rtm-largest-table-size* *rtm-initial-table-size*)

;;;
;;; Data Structures, and Constructor/Accessor macros:
;;;

;;
;; Domain Data Structures and Macros:
;;

(defmacro get-domain (domain-name)
  `(gethash ,domain-name *rtm-domains*))

(defun domain-encode-and-verify (domain-name attr-value)
  "Returns the internal form of attr-value after encoding and verifying 
it for specified domain.  If attr-value is not valid for domain, a warning
is generated and the default domain value is returned."
  (let ((domain (get-domain domain-name))
	(int-value attr-value))
    (if domain
	(let ((encode-fn (domain-encode-fn domain))
	      (verify-fn (domain-verify-fn domain))
	      (default-value (domain-default-value domain)))
	  (when encode-fn
	    (setf int-value (funcall encode-fn attr-value)))
	  (when (and verify-fn (not (funcall verify-fn int-value)))
	    (rtm-warning "Attribute value \'" attr-value
			 " is invalid for domain \'" domain-name " - using default value \'"
			 (domain-decode domain-name default-value) " instead.")
	    (setf int-value default-value)))
	(rtm-error "Undefined domain \'" domain-name "."))
    (values int-value)))

(defun domain-decode (domain-name attr-value)
  "Returns external form of attr-value after decoding it for specified domain."
  (let ((domain (get-domain domain-name))
	(ext-value attr-value))
    (if domain
	(let ((decode-fn (domain-decode-fn domain)))
	  (setf ext-value
		(if decode-fn
		    (funcall decode-fn attr-value)
		    attr-value)))
	(rtm-error "Undefined domain \'" domain-name "."))
    (values ext-value)))

;;
;; Relation Data Structures and Macros:
;;

(defstruct table
  (name nil)
  (attr-name-list nil)
  (free-tuple-chain nil)
  (degree nil)
  (cardinality nil)
  (tuple-count nil)
  (indexed-attr-name-list nil)
  (change-count 0))

(defmacro get-table (tbl-name)
  `(gethash ,tbl-name *rtm-relations*))

(defmacro rem-table (tbl-name)
  `(remhash ,tbl-name *rtm-relations*))

(defun rtm-tables ()
  (let (table-name-list)
    (maphash #'(lambda (key val) val (push key table-name-list)) 
             *rtm-relations*)
    (values table-name-list)))

;;
;; View Data Structures and Macros:
;;

(defstruct view
  (base-table-name nil)
  (tuple-list nil)
  (where-clause nil)
  (change-count 0))

(defmacro get-view (view-name)
  `(gethash ,view-name *rtm-relation-views*))

(defmacro rem-view (view-name)
  `(remhash ,view-name *rtm-relation-views*))

(defun rtm-views ()
  (let (view-name-list)
    (maphash #'(lambda (key val) val (push key view-name-list)) 
             *rtm-relation-views*)
    (values view-name-list)))

;;
;; Sample Data Structures and Macros:
;;

(defstruct sample
  (base-table-name nil)
  (tuple-array nil))

(defmacro get-sample (sample-name)
  `(gethash ,sample-name *rtm-relation-samples*))

(defmacro rem-sample (sample-name)
  `(remhash ,sample-name *rtm-relation-samples*))

(defun rtm-samples ()
  (let (sample-name-list)
    (maphash #'(lambda (key val) val (push key sample-name-list)) 
             *rtm-relation-samples*)
    (values sample-name-list)))

;;
;; Attribute Data Structures and Macros:
;;

(defstruct attribute
  (name nil)
  (domain-name nil)
  (data nil)
  (index nil)
  (key-p nil)
  (ordered-p nil))

(defmacro get-attribute (table-attr-name)
  `(gethash ,table-attr-name *rtm-attributes*))

(defmacro rem-attribute (table-attr-name)
  `(remhash ,table-attr-name *rtm-attributes*))

;;
;; Relation-Attribute Pair Structures and Macros:
;;

(defmacro table-attribute-name (tbl-name attr-name)
  `(cons ,tbl-name ,attr-name))

(defvar *current-table-attribute-name* (cons nil nil))

(defmacro current-table-name ()
  `(car *current-table-attribute-name*))

(defmacro current-table ()
  `(get-table (current-table-name)))

(defmacro current-attribute-name ()
  `(cdr *current-table-attribute-name*))

(defmacro current-attribute (attr-name)
  `(progn
     (setf (current-attribute-name) ,attr-name)
     (get-attribute *current-table-attribute-name*)))

(defmacro current-domain (attr-name)
  `(get-domain (attribute-domain-name (current-attribute ,attr-name))))

(defun domain-name (tbl-name attr-name)
  (attribute-domain-name (get-attribute (table-attribute-name tbl-name attr-name))))

(defun domain-numeric-p (tbl-name attr-name)
  (when (member (domain-name tbl-name attr-name)
                '(:fixnum :integer :float :single-float :double-float :number))
    t))

;;
;; Table-scope data structures, functions, and macros:
;;

(defvar *table-scope-stack* nil)

(defmacro make-scope-record (&key base-table-name (view-or-sample nil))
  `(cons ,base-table-name ,view-or-sample))

(defmacro current-scope-table-name ()
  `(car (first *table-scope-stack*)))

(defmacro current-scope-view-or-sample ()
  `(cdr (first *table-scope-stack*)))

(defun push-table-scope (tbl-name &optional (allow-sample-p nil))
  "Create table scope record and push on scope stack.  If tbl-name is a
view, set base-table-name and view in stack record.  Otherwise, just set 
base-table-name.  Returns t if successful or nil if error."
  (let ((view-or-sample (or (get-view tbl-name) (get-sample tbl-name)))
	(base-table-name tbl-name))
    (cond ((view-p view-or-sample)
	   (setf base-table-name (view-base-table-name view-or-sample)))
          ((and allow-sample-p (sample-p view-or-sample))
           (setf base-table-name (sample-base-table-name view-or-sample)))
	  ((not (get-table tbl-name))
	   (rtm-warning "Table \'" tbl-name " doesn't exist.")
	   (return-from push-table-scope (values nil))))
    (push (make-scope-record :base-table-name base-table-name :view-or-sample view-or-sample)
	  *table-scope-stack*)
    (setf (current-table-name) base-table-name)
    (values t)))

(defun pop-table-scope ()
  "Restore previous table scope by popping scope stack."
  (pop *table-scope-stack*)
  (setf (current-table-name) (current-scope-table-name))
  (values t))

;;;
;;; Tuple-Set data structures and functions
;;;   to support fast set operations:
;;;     Set membership can be tested in O(1) time.
;;;     Set intersection and union take O(n) time
;;;         for input set of size n.
;;;     A set workspace is maintained for each nesting
;;;         level of query select where-clause.
;;;     Allocation of set workspace is as needed.
;;;     Sets grow in size with base tables.
;;;

(defparameter *rtm-tuple-set-counter-threshold* 
	      (round (/ most-positive-fixnum 2)))

(defstruct tuple-set
  (chain 0)
  (counter 0)
  (cardinality 0)
  (value-array
    (make-array (list (1+ *rtm-largest-table-size*))
		:element-type 'fixnum :initial-element 0 :adjustable t))
  (chain-array
    (make-array (list (1+ *rtm-largest-table-size*))
		:element-type 'fixnum :initial-element 0 :adjustable t)))

;;
;; Tuple-set accessor macros and functions:
;;

(defun init-tuple-sets ()
  (setf *rtm-tuple-sets* nil))

(defmacro tuple-set-empty-p (tuple-set)
  `(zerop (tuple-set-chain ,tuple-set)))

(defmacro tuple-set-member-p (tuple-set tuple-number)
  `(= (tuple-set-counter ,tuple-set)
      (aref (tuple-set-value-array ,tuple-set) ,tuple-number)))

(defmacro do-tuple-set ((tuple-number tuple-set) &body body)
  `(do ((,tuple-number (tuple-set-chain ,tuple-set)
	 (aref (tuple-set-chain-array ,tuple-set) ,tuple-number)))
       ((= ,tuple-number 0))
     ,@body))

(defun tuple-set-list (tuple-set)
  "Return list of tuple-set members."
  ;; Returns list of current tuple set entries by traversing the chain
  ;;   in chain-array of tuple-set until a tuple number of 0 is reached.
  (let ((tuple-number-list nil))
    (do-tuple-set (tuple-number tuple-set)
      (push tuple-number tuple-number-list))
    (values tuple-number-list)))

(defun tuple-set-array (tuple-set)
  "Return array of tuple-set members."
  ;; Returns array of current tuple set entries by traversing the chain
  ;;   in chain-array of tuple-set until a tuple number of 0 is reached.
  (let ((tuple-number-array (make-array (tuple-set-cardinality tuple-set)
                                        :element-type 'fixnum))
        (i 0))
    (do-tuple-set (tuple-number tuple-set)
      (setf (aref tuple-number-array i) tuple-number)
      (incf i))
    (values tuple-number-array)))

(defun tuple-set-copy (tuple-set)
  "Return a copy of tuple-set."
  ;; Allocate a new empty tuple-set, and union in members of specified tuple-set.
  (let ((copy-of-tuple-set (tuple-set-allocate)))
    (do-tuple-set (tuple-number tuple-set)
      (tuple-set-union-element copy-of-tuple-set tuple-number))
    (values copy-of-tuple-set)))

;;
;; Tuple-set functions for memory management (storage allocation and release):
;;

(defun tuple-set-allocate ()
  "Return an empty tuple-set of size greater than or equal to *rtm-largest-table-size*."
  ;; Use next empty tuple set, making sure it has at least *rtm-largest-table-size* entries,
  ;;  or create a new tuple-set if next free set is nil.
  (let ((cardinality (1+ *rtm-largest-table-size*))
	(tuple-set (first *rtm-tuple-sets*)))
    ;; If no more tuple sets on list, one is created.
    (if tuple-set
	(setf *rtm-tuple-sets* (rest *rtm-tuple-sets*))
	(setf tuple-set (make-tuple-set)))
    ;; If existing size is too small, set is expanded.
    (when (< (array-total-size (tuple-set-value-array tuple-set)) cardinality)
      (setf (tuple-set-value-array tuple-set)
	    (adjust-array (tuple-set-value-array tuple-set) cardinality
			  :element-type 'fixnum :initial-element 0))
      (setf (tuple-set-chain-array tuple-set)
	    (adjust-array (tuple-set-chain-array tuple-set) cardinality
			  :element-type 'fixnum :initial-element 0)))
    ;; If counter is getting too big, counter and arrays are reinitialized. 
    (when (>= (tuple-set-counter tuple-set) *rtm-tuple-set-counter-threshold*)
      (setf (tuple-set-counter tuple-set) 0)
      (dotimes (i (array-total-size (tuple-set-value-array tuple-set)))
	(setf (aref (tuple-set-value-array tuple-set) i) 0)
	(setf (aref (tuple-set-chain-array tuple-set) i) 0)))
    ;; Make tuple-set empty by incrementing counter and setting chain and cardinality to 0.
    (incf (tuple-set-counter tuple-set))
    (setf (tuple-set-chain tuple-set) 0
	  (tuple-set-cardinality tuple-set) 0)
    tuple-set))

(defun tuple-set-release (tuple-set)
  "Return tuple-set to pool of available tuple-sets."
  ;; Push released tuple-set onto list of free tuple-sets.
  (push tuple-set *rtm-tuple-sets*)
  nil)

;;
;; Tuple-set functions for set intersection:
;;

(defun tuple-set-intersect-set (tuple-set-1 tuple-set-2)
  "Return a tuple-set containing intersection of members from tuple-set-1 and  tuple-set-2."
  ;; Intersect tuple-set-1 with tuple-set-2.
  ;; Release storage for one tuple-set and return the other.
  (let (source-ts target-ts)
    ;; Smaller tuple-set is the Source, and larger tuple-set is the Target.
    (if (< (tuple-set-cardinality tuple-set-1)
	   (tuple-set-cardinality tuple-set-2))
	(setf source-ts tuple-set-1
	      target-ts tuple-set-2)
	(setf source-ts tuple-set-2
	      target-ts tuple-set-1))
    (if (tuple-set-empty-p source-ts)
	;; Short circuit if smallest tuple-set (source) is empty.
	(progn
	  (tuple-set-release target-ts)
	  source-ts)
	;; Iterate over the smaller of the two sets, intersecting into the other.
	(let ((prev-tuple-number 0))
	  (setf (tuple-set-cardinality target-ts) 0)
	  ;; Traverse entries in source tuple-set.
	  (do-tuple-set (source-tuple-number source-ts)
	    ;; Tuple numbers are added to new chain only if they are in current set.
	    (when (tuple-set-member-p target-ts source-tuple-number)
	      (incf (aref (tuple-set-value-array target-ts) source-tuple-number))
	      (incf (tuple-set-cardinality target-ts))
	      (setf (aref (tuple-set-chain-array target-ts) source-tuple-number)
		    prev-tuple-number)
	      (setf prev-tuple-number source-tuple-number)))
	  ;; Counter is incremented to exclude old set members.
	  (incf (tuple-set-counter target-ts))
	  (setf (tuple-set-chain target-ts) prev-tuple-number)
	  ;; Release storage for source tuple-set and return target containing intersection.
	  (tuple-set-release source-ts)
	  target-ts))))

(defun tuple-set-intersect-set-fast (source-ts target-ts)
  "Return target tuple-set containing intersection of members of source-ts and target-ts."
  ;; Intersect source-ts into target-ts, and release storage for source-ts.
  ;; Assumes source-ts is smaller of two, but non-empty.
  (let ((prev-tuple-number 0))
    (setf (tuple-set-cardinality target-ts) 0)
    ;; Traverse entries in source tuple-set.
    (do-tuple-set (source-tuple-number source-ts)
      ;; Tuple numbers are added to new chain only if they are in current set.
      (when (tuple-set-member-p target-ts source-tuple-number)
	(incf (aref (tuple-set-value-array target-ts) source-tuple-number))
	(incf (tuple-set-cardinality target-ts))
	(setf (aref (tuple-set-chain-array target-ts) source-tuple-number) prev-tuple-number)
	(setf prev-tuple-number source-tuple-number)))
    ;; Counter is incremented to exclude old set members.
    (incf (tuple-set-counter target-ts))
    (setf (tuple-set-chain target-ts) prev-tuple-number)
    ;; Release storage for source tuple-set and return target containing intersection.
    (tuple-set-release source-ts)
    target-ts))

(defun tuple-set-intersect-list (tuple-set tuple-number-list)
  "Return tuple-set containing intersection of members of tuple-set and tuple-number-list."
  ;; Intersect tuple-set with tuple-number-list, returning tuple-set.
  ;; Short circuit if tuple set is empty.
  (unless (tuple-set-empty-p tuple-set)
    (let ((prev-tuple-number 0))
      (setf (tuple-set-cardinality tuple-set) 0)
      ;; Traverse entries in tuple-number-list.
      (dolist (input-tuple-number tuple-number-list)
	;; Tuple numbers are added to new chain only if they are in current set.
	(when (tuple-set-member-p tuple-set input-tuple-number)
	  (incf (aref (tuple-set-value-array tuple-set) input-tuple-number))
	  (incf (tuple-set-cardinality tuple-set))
	  (setf (aref (tuple-set-chain-array tuple-set) input-tuple-number) prev-tuple-number)
	  (setf prev-tuple-number input-tuple-number)))
      ;; Counter is incremented to exclude old set members.
      (incf (tuple-set-counter tuple-set))
      (setf (tuple-set-chain tuple-set) prev-tuple-number)))
  ;; Return set containing intersection.
  tuple-set)

;;
;; Tuple-set functions for set union:
;;

(defun tuple-set-union-set (tuple-set-1 tuple-set-2)
  "Return tuple-set containing the union of members from tuple-set-1 and  tuple-set-2."
  ;; Union tuple-set-1 with tuple-set-2.
  ;; Release storage for one tuple-set and return the other.
  (let (source-ts target-ts)
    ;; Smaller tuple-set is the Source, and larger tuple-set is the Target.
    (if (< (tuple-set-cardinality tuple-set-1) (tuple-set-cardinality tuple-set-2))
	(setf source-ts tuple-set-1
	      target-ts tuple-set-2)
	(setf source-ts tuple-set-2
	      target-ts tuple-set-1))
    ;; Short circuit if smallest tuple-set is empty.
    (unless (tuple-set-empty-p source-ts)
      ;; Iterate over the smaller of the two sets, unioning into the other.
      (let ((counter (tuple-set-counter target-ts))
	    (prev-tuple-number (tuple-set-chain target-ts)))
	;; Traverse entries in source tuple-set.
	(do-tuple-set (source-tuple-number source-ts)
	  ;; Tuple numbers are added to existing chain only if they are not in current set.
	  (unless (tuple-set-member-p target-ts source-tuple-number)
	    (setf (aref (tuple-set-value-array target-ts) source-tuple-number) counter)
	    (incf (tuple-set-cardinality target-ts))
	    (setf (aref (tuple-set-chain-array target-ts) source-tuple-number) prev-tuple-number)
	    (setf prev-tuple-number source-tuple-number)))
	;; Counter is not incremented since set is only added to.
	(setf (tuple-set-chain target-ts) prev-tuple-number)))
    ;; Release storage for source tuple-set and return target containing union.
    (tuple-set-release source-ts)
    target-ts))

(defun tuple-set-union-set-fast (target-ts source-ts)
  "Return target tuple-set containing union of members of source-ts and target-ts."
  ;; Union source-ts into target-ts, and release storage for source-ts.
  ;; Assumes source-ts is smaller of two, but non-empty.
  (let ((counter (tuple-set-counter target-ts))
	(prev-tuple-number (tuple-set-chain target-ts)))
    ;; Traverse entries in source tuple-set.
    (do-tuple-set (source-tuple-number source-ts)
      ;; Tuple numbers are added to existing chain only if they are not in current set.
      (unless (tuple-set-member-p target-ts source-tuple-number)
	(setf (aref (tuple-set-value-array target-ts) source-tuple-number) counter)
	(incf (tuple-set-cardinality target-ts))
	(setf (aref (tuple-set-chain-array target-ts) source-tuple-number) prev-tuple-number)
	(setf prev-tuple-number source-tuple-number)))
    ;; Counter is not incremented since set is only added to.
    (setf (tuple-set-chain target-ts) prev-tuple-number)
    ;; Release storage for source tuple-set and return target containing union.
    (tuple-set-release source-ts)
    target-ts))

(defun tuple-set-union-list (tuple-set tuple-number-list)
  "Return tuple-set containing union of members of tuple-set and tuple-number-list."
  ;; Union tuple-set with tuple-number-list, returning tuple-set.
  (let ((counter (tuple-set-counter tuple-set))
	(prev-tuple-number (tuple-set-chain tuple-set)))
    ;; Traverse entries in tuple-number-list.
    (dolist (input-tuple-number tuple-number-list)
      ;; Tuple numbers are added to existing chain only if they are not in current set.
      (unless (tuple-set-member-p tuple-set input-tuple-number)
	(setf (aref (tuple-set-value-array tuple-set) input-tuple-number) counter)
	(incf (tuple-set-cardinality tuple-set))
	(setf (aref (tuple-set-chain-array tuple-set) input-tuple-number) prev-tuple-number)
	(setf prev-tuple-number input-tuple-number)))
    ;; Counter is not incremented since set is only added to.
    (setf (tuple-set-chain tuple-set) prev-tuple-number)
    ;; Return set containing union.
    tuple-set))

(defun tuple-set-union-element (tuple-set tuple-number)
  "Return tuple-set containing union of members of tuple-set and tuple-number."
  ;; Union tuple-set with tuple-number, returning tuple-set.
  ;; Tuple number is added to existing chain only if it is not in current set.
  (unless (tuple-set-member-p tuple-set tuple-number)
    (setf (aref (tuple-set-value-array tuple-set) tuple-number)
	  (tuple-set-counter tuple-set))
    (incf (tuple-set-cardinality tuple-set))
    (setf (aref (tuple-set-chain-array tuple-set) tuple-number)
	  (tuple-set-chain tuple-set))
    ;; Counter is not incremented since set is only added to.
    (setf (tuple-set-chain tuple-set) tuple-number))
  ;; Return set containing union.
  tuple-set)

;;;
;;; Standard (built-in) Domain Definitions:
;;;

(defun fixnum-encode (value)
  "Coerce value to type fixnum."
  (when (and (typep value 'number)
	     (not (typep value 'complex)))
    (coerce (round value) 'fixnum)))

(defun integer-encode (value)
  "Coerce value to type integer."
  (when (and (typep value 'number)
	     (not (typep value 'complex)))
    (coerce (round value) 'integer)))

(defun float-encode (value)
  "Coerce value to type float."
  (when (and (typep value 'number)
	     (not (typep value 'complex)))
    (coerce value 'float)))

(defun single-float-encode (value)
  "Coerce value to type single-float."
  (when (and (typep value 'number)
	     (not (typep value 'complex)))
    (coerce value 'single-float)))

(defun double-float-encode (value)
  "Coerce value to type double-float."
  (when (and (typep value 'number)
	     (not (typep value 'complex)))
    (coerce value 'double-float)))

(defun symbol-encode (symbol)
  "Convert symbol into uppercase string."
  (when (symbolp symbol)
    (string-upcase (string symbol))))

(defun symbol-decode (string)
  "Convert string back into a symbol."
  (intern string))

(defun lowercase-string-encode (string)
  "Convert string to all lower case."
  (when (stringp string)
    (string-downcase string)))

(defun uppercase-string-encode (string)
  "Convert string to all upper case."
  (when (stringp string)
    (string-upcase string)))

;;;
;;; RTM relations (base tables) are defined by a set of 1 dimensional arrays.
;;;    In the relation stucture, a free-tuple chain array stores either an index to
;;;    the next available row for insertion, or nil if the row is taken.  The last
;;;    free tuple pointer is zero.
;;;
;;; The table attributes each have their own structure, containing the column's data
;;;    array itself, as well as some auxilary information including the column's 
;;;    index, if any.  The degree of the table is the number of columns, and the
;;;    cardinality is the number of allocated rows.
;;;

;;; Primitive Table accessor macros and functions:

(defmacro first-free-tuple-pos (free-tuple-chain)
  `(aref ,free-tuple-chain 0))

(defmacro next-free-tuple-pos (free-tuple-chain row)
  `(aref ,free-tuple-chain ,row))

(defmacro free-tuple-p (free-tuple-chain row)
  `(aref ,free-tuple-chain ,row))

(defmacro do-table-rows ((row free-tuple-chain) &body body)
  `(do ((,row 1 (1+ ,row)))
       ((>= ,row (array-dimension ,free-tuple-chain 0)))
     ,@body))

(defmacro do-table-filled-rows ((row free-tuple-chain) &body body)
  `(do ((,row 1 (1+ ,row)))
       ((>= ,row (array-dimension ,free-tuple-chain 0)))
     (unless (free-tuple-p ,free-tuple-chain ,row)
       ,@body)))

(defmacro do-attributes ((attr table &optional (attrs nil)) &body body)
  "Iterate over attribute structures of table."
  (let ((tbl-name (gensym "TBL-NAME"))
        (attr-name (gensym "ATTR-NAME"))
        (tbl-attr-name (gensym "TBL-ATTR-NAME")))
    `(let* ((,attr nil)
            (,tbl-name (table-name ,table))
            (,tbl-attr-name (table-attribute-name ,tbl-name nil)))
       (dolist (,attr-name (or ,attrs (table-attr-name-list ,table)))
         (setf (cdr ,tbl-attr-name) ,attr-name
               ,attr (get-attribute ,tbl-attr-name))
         ,@body))))

(defun table-tuple-get (table row &optional (attrs nil))
  "Return list of attribute values of selected row of the current table."
  (let ((attr-list (or attrs (table-attr-name-list table)))
        (tuple nil))
    (dolist (attr-name attr-list)
      (push (aref (attribute-data (current-attribute attr-name)) row) tuple))
    (values tuple)))

(defun table-mark-as-changed (tbl-name)
  (incf (table-change-count (get-table tbl-name))))

(defun table-unmark-as-changed (tbl-name)
  (setf (table-change-count (get-table tbl-name)) 0))

;;;
;;; Query functions and macros for attribute comparison operators:
;;; 

(defmacro attr-eq-p (attr-value-1 attr-value-2 equal-p-fn less-p-fn)
  (declare (ignore less-p-fn))
  `(funcall ,equal-p-fn ,attr-value-1 ,attr-value-2))

(defmacro attr-ne-p (attr-value-1 attr-value-2 equal-p-fn less-p-fn)
  (declare (ignore less-p-fn))
  `(not (funcall ,equal-p-fn ,attr-value-1 ,attr-value-2)))

(defmacro attr-lt-p (attr-value-1 attr-value-2 equal-p-fn less-p-fn)
  (declare (ignore equal-p-fn))
  `(funcall ,less-p-fn ,attr-value-1 ,attr-value-2))

(defmacro attr-ge-p (attr-value-1 attr-value-2 equal-p-fn less-p-fn)
  (declare (ignore equal-p-fn))
  `(not (funcall ,less-p-fn ,attr-value-1 ,attr-value-2)))

(defmacro attr-le-p (attr-value-1 attr-value-2 equal-p-fn less-p-fn)
  `(or (funcall ,less-p-fn ,attr-value-1 ,attr-value-2)
       (funcall ,equal-p-fn ,attr-value-1 ,attr-value-2)))

(defmacro attr-gt-p (attr-value-1 attr-value-2 equal-p-fn less-p-fn)
  `(not (or (funcall ,less-p-fn ,attr-value-1 ,attr-value-2)
	    (funcall ,equal-p-fn ,attr-value-1 ,attr-value-2))))

(defmacro attr-ge-le-p (table-attr-value attr-value-1 attr-value-2
			  equal-p-fn less-p-fn)
  `(and (not (funcall ,less-p-fn ,table-attr-value ,attr-value-1))
	(attr-le-p ,table-attr-value ,attr-value-2 ,equal-p-fn ,less-p-fn)))

(defmacro attr-gt-le-p (table-attr-value attr-value-1 attr-value-2
			  equal-p-fn less-p-fn)
  `(and (attr-gt-p ,table-attr-value ,attr-value-1 ,equal-p-fn ,less-p-fn)
	(attr-le-p ,table-attr-value ,attr-value-2 ,equal-p-fn ,less-p-fn)))

(defmacro attr-ge-lt-p (table-attr-value attr-value-1 attr-value-2
			  equal-p-fn less-p-fn)
  (declare (ignore equal-p-fn))
  `(and (not (funcall ,less-p-fn ,table-attr-value ,attr-value-1))
	(funcall ,less-p-fn ,table-attr-value ,attr-value-2)))

(defmacro attr-gt-lt-p (table-attr-value attr-value-1 attr-value-2
			  equal-p-fn less-p-fn)
  `(and (attr-gt-p ,table-attr-value ,attr-value-1 ,equal-p-fn ,less-p-fn)
	(funcall ,less-p-fn ,table-attr-value ,attr-value-2)))

(defmacro attr-le-ge-p (table-attr-value attr-value-1 attr-value-2
			  equal-p-fn less-p-fn)
  `(or (attr-le-p ,table-attr-value ,attr-value-1 ,equal-p-fn ,less-p-fn)
       (not (funcall ,less-p-fn ,table-attr-value ,attr-value-2))))

(defmacro attr-lt-ge-p (table-attr-value attr-value-1 attr-value-2
			  equal-p-fn less-p-fn)
  (declare (ignore equal-p-fn))
  `(or (funcall ,less-p-fn ,table-attr-value ,attr-value-1)
       (not (funcall ,less-p-fn ,table-attr-value ,attr-value-2))))

(defmacro attr-le-gt-p (table-attr-value attr-value-1 attr-value-2
			  equal-p-fn less-p-fn)
  `(or (attr-le-p ,table-attr-value ,attr-value-1 ,equal-p-fn ,less-p-fn)
       (attr-gt-p ,table-attr-value ,attr-value-2 ,equal-p-fn ,less-p-fn)))

(defmacro attr-lt-gt-p (table-attr-value attr-value-1 attr-value-2
			  equal-p-fn less-p-fn)
  `(or (funcall ,less-p-fn ,table-attr-value ,attr-value-1)
       (attr-gt-p ,table-attr-value ,attr-value-2 ,equal-p-fn ,less-p-fn)))

