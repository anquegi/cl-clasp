;;; -*- Mode:Common-Lisp; Base:10 -*-
;;;; *-* Last-edit: Monday, July 27, 1992  22:42:23; Edited-By: LOISELLE *-* 

;;; Version 2.1, June 1992.
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

;;; -- * --
;;;
;;; Attribute checking function:
;;;

(defun table-attributes-p (tbl-name attr-name-list)
  "Predicate to test if attributes are valid in table."
  (when (push-table-scope tbl-name)
    (let ((result t))
      (unless (listp attr-name-list)
	(setf attr-name-list (list attr-name-list)))
      (dolist (attr-name attr-name-list)
	(unless (current-attribute attr-name)
	  (rtm-warning "Invalid attribute \'" attr-name " for table \'" tbl-name ".")
	  (setf result nil)
	  (return)))
      (pop-table-scope)
      (values result))))

;;;
;;; RTM relation (base table) tuple maintenance functions.
;;;

(defun init-free-tuple-chain (cardinality)
  "Allocate, initialize and return free tuple chain array."
  (let ((free-tuple-chain (make-array (1+ cardinality)
                                      :element-type 'integer 
                                      :adjustable t)))
    ;; Chain together all tuples (all are free initially)
    (do ((row 1 (1+ row)))
	((>= row cardinality)
	 (setf (next-free-tuple-pos free-tuple-chain row) 0))
      (setf (next-free-tuple-pos free-tuple-chain row) (1+ row)))
    (setf (first-free-tuple-pos free-tuple-chain) 1)
    ;; If table is now largest in system, update *rtm-largest-table-size*.
    (when (> cardinality *rtm-largest-table-size*)
      (setf *rtm-largest-table-size* cardinality))
    free-tuple-chain))

(defun table-tuple-allocate (table)
  "Allocate first available tuple for current table 
and return its position if successful or nil if error."
  (let* ((free-tuple-chain (table-free-tuple-chain table))
	 (tuple-pos (first-free-tuple-pos free-tuple-chain)))
    ;; If no table tuple positions are available, expand the table cardinality
    ;;   by *rtm-table-expansion-factor*.
    (when (zerop tuple-pos)
      (let* ((old-cardinality (table-cardinality table))
	     (new-cardinality (round (* old-cardinality *rtm-table-expansion-factor*)))
             (new-array-size (1+ new-cardinality)))
	(when (> new-array-size array-total-size-limit)
	  (rtm-warning "Base table array too large for expansion.")
	  (return-from table-tuple-allocate (values nil)))
	;; If not too big already, expand table and update table record.
	(setf tuple-pos (1+ old-cardinality))
	(setf free-tuple-chain (adjust-array free-tuple-chain new-array-size))
	(do ((row tuple-pos (1+ row)))
	    ((>= row new-cardinality)
	     (setf (next-free-tuple-pos free-tuple-chain row) 0))
	  (setf (next-free-tuple-pos free-tuple-chain row) (1+ row)))
	(setf (first-free-tuple-pos free-tuple-chain) tuple-pos)
        ;; Expand each attribute data column array.
        (do-attributes (attribute table)
          (setf (attribute-data attribute)
                (adjust-array (attribute-data attribute) new-array-size)))
	;; Update table record in system hash table.
	(setf (table-free-tuple-chain table) free-tuple-chain
              (table-cardinality table) new-cardinality)
	;; If table is now largest in system, update *rtm-largest-table-size*.
	(when (> new-cardinality *rtm-largest-table-size*)
	  (setf *rtm-largest-table-size* new-cardinality))))
    ;; Update free tuple list, mark next unused table tuple as allocated, and return tuple position.
    (setf (first-free-tuple-pos free-tuple-chain)
	  (next-free-tuple-pos free-tuple-chain tuple-pos))
    tuple-pos))

(defun table-tuple-release (table tuple-pos)
  "Release specified tuple and update free tuple list."
  (let ((free-tuple-chain (table-free-tuple-chain table)))
    ;; Update free tuple chain, marking tuple as free by linking it in at head of chain.
    (setf (next-free-tuple-pos free-tuple-chain tuple-pos)
	  (first-free-tuple-pos free-tuple-chain))
    (setf (first-free-tuple-pos free-tuple-chain) tuple-pos)))

;;;
;;; RTM relation (base table) tuple insertion and deletion functions.
;;;

(defun table-insert-tuple (tbl-name tuple)
  "Insert table tuple using ordered unencoded attribute values in tuple,
update necessary indices, and return t if successful or nil if error.
Used for table load from disk file."
  (let ((table (get-table tbl-name))
	tuple-number)
    ;; Check for table size overflow error.
    (unless (setf tuple-number (table-tuple-allocate table))
      (rtm-warning "Insert of tuple \'" tuple
		   " into table \'" tbl-name " failed - table too large.")
      (return-from table-insert-tuple (values nil)))
    ;; If no simple errors, encode and verify attribute-values.
    (let ((attr-value-list tuple)
	  tuple-set)
      ;; Iterate over all table columns, and insert attr-value.  Also check uniqueness of key.
      (do-attributes (attribute table)
	(let* ((attr-name (attribute-name attribute))
	       (int-attr-value (domain-encode-and-verify
				 (attribute-domain-name attribute) (first attr-value-list))))
	  (setf attr-value-list (rest attr-value-list))
	  ;; If attribute is part of key, intersect into tuple-set those 
          ;;    tuple-numbers with matching values.
	  (when (attribute-key-p attribute)
	    (if tuple-set
		(setf tuple-set (tuple-set-intersect-set
				  tuple-set (query-eq attr-name int-attr-value)))
		(setf tuple-set (query-eq attr-name int-attr-value))))
	  ;; Insert internal attribute value in newly allocated tuple.
	  (setf (aref (attribute-data attribute) tuple-number) int-attr-value)))
      ;; If tuple-set is not empty, then tuple has non-unique set of key attributes.
      (unless (or (null tuple-set) (tuple-set-empty-p tuple-set))
	(rtm-warning "Insert of tuple \'" tuple
		     " into table \'" tbl-name " failed - non-unique key.")
	(tuple-set-release tuple-set)
	(table-tuple-release table tuple-number)
	(return-from table-insert-tuple (values nil)))
      (when tuple-set (tuple-set-release tuple-set))
      ;; Tuple is OK, so mark tuple allocated, increment table tuple count, 
      ;;   mark base table as changed, and update indices.
      (setf (next-free-tuple-pos (table-free-tuple-chain table) tuple-number) nil)
      (incf (table-tuple-count table))
      (table-mark-as-changed tbl-name)
      (do-attributes (attribute table (table-indexed-attr-name-list table))
	(index-insert (attribute-name attribute)
		      (aref (attribute-data attribute) tuple-number)
		      tuple-number))
      ;; Return success.
      (values t))))

(defun table-delete-tuples (tbl-name tuple-set)
  "Delete selected rows and return t if successful or nil if error."
  (let ((table (get-table tbl-name)))
    ;; Check for non-existing base table error.
    (unless table
      (rtm-warning "Delete from table \'" tbl-name " failed - table doesn't exist.")
      (return-from table-delete-tuples (values nil)))
    ;; If no simple errors, collect data columns for indexed attributes
    (let ((attr-name-list (table-indexed-attr-name-list table))
          (data-array-list nil))
      (do-attributes (attribute table attr-name-list)
        (push-end (attribute-data attribute) data-array-list))
      ;; Iterate over tuples to be deleted.
      (do-tuple-set (tuple-number tuple-set)
        ;; Delete index references for this tuple.
        (do ((attr-names attr-name-list (rest attr-names))
             (data-arrays data-array-list (rest data-array-list)))
            ((null attr-names))
          (index-delete (first attr-names) 
                        (aref (first data-arrays) tuple-number)
                        tuple-number))
	;; Decrement tuple count and release tuple.
	(decf (table-tuple-count table))
	(table-tuple-release table tuple-number))
      ;; Release tuple-set.
      (tuple-set-release tuple-set)
      ;; Return success.
      (values t))))

;;;
;;; RTM relation (base table) attribute value accessor and modifier functions.
;;;

(defun table-element-access (row attr-name)
  "Function used by attr-value to access attribute values in current table.
Returns value if successful, or nil if error."
  (let ((attribute (current-attribute attr-name)))
    ;; Validate attr-name for current table.
    (unless attribute
      (rtm-warning "Attribute value access returning nil - invalid attribute \'" attr-name
		   " for table \'" (current-table-name) ".")
      (return-from table-element-access (values nil)))
    ;; Decode internal attribute value for domain, and return external value.
    (values (domain-decode (attribute-domain-name attribute) 
                           (aref (attribute-data attribute) row)))))
  
(defun table-element-modify (row attr-name attr-value)
  "Function used by setf of attr-value to update attribute values in current table.
Returns t if successful, or nil if error."
  (let ((attribute (current-attribute attr-name)))
    ;; Validate attr-name for current table.
    (unless attribute
      (rtm-warning "Attribute value update failed - invalid attribute \'" attr-name
		   " for table \'" (current-table-name) ".")
      (return-from table-element-modify (values nil)))
    ;; Check that current table is not a sample.
    (when (sample-p (current-scope-view-or-sample))
      (rtm-warning "Attribute value update failed - \'" (current-table-name) " is a sample.")
      (return-from table-element-modify (values nil)))
    ;; Encode external attribute value for domain, and return internal value.
    (let* ((domain-name (attribute-domain-name attribute))
	   (indexed-p (attribute-index attribute))
           (data-array (attribute-data attribute))
	   (old-attr-value (aref data-array row))
	   (new-attr-value (domain-encode-and-verify domain-name attr-value))
	   tuple-set)
      ;; Insert new-attr-value and update index if necessary.
      (setf (aref data-array row) new-attr-value)
      (when indexed-p 
	(index-delete attr-name old-attr-value row)
	(index-insert attr-name new-attr-value row))
      ;; Check for uniqueness of key.
      (do-attributes (attribute (current-table))
	(let* ((attr-name (attribute-name attribute))
	       (int-attr-value (aref (attribute-data attribute) row)))
	  ;; If attribute is part of key, intersect into tuple-set those 
          ;;    tuple-numbers with matching values.
	  (when (attribute-key-p attribute)
	    (if tuple-set
		(setf tuple-set (tuple-set-intersect-set
				  tuple-set (query-eq attr-name int-attr-value)))
		(setf tuple-set (query-eq attr-name int-attr-value))))))
      ;; If tuple-set is not empty, then tuple has non-unique set of key attributes.
      (unless (or (null tuple-set) (= 1 (tuple-set-cardinality tuple-set)))
	(rtm-warning "Attribute value update failed for attribute \'" attr-name
		     ", value \'" new-attr-value
		     ", in table \'" (current-table-name) " - non-unique key.")
	(tuple-set-release tuple-set)
	(setf (aref data-array row) old-attr-value)
	(when indexed-p
	  (index-delete attr-name new-attr-value row)
	  (index-insert attr-name old-attr-value row))
	(return-from table-element-modify (values nil)))
      (when tuple-set (tuple-set-release tuple-set))
      ;; Return success.
      (values t))))

;;;
;;; Selection forms use (attr-value attr-name) to access table elments
;;;   and (setf (attr-value attr-name) value) to modify table elements.
;;;   (see definitions for do-selection and do-neighbors)
;;;
(defsetf table-element-access table-element-modify)
