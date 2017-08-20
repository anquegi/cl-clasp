;;; -*- Mode:Common-Lisp; Base:10 -*-
;;;; *-* Last-edit: Monday, February 8, 1993  18:59:20; Edited-By: carlson *-* 

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

(export '(create-table rename-table drop-table rtm-table-p table-attributes
          create-view drop-view rtm-view-p
          create-sample drop-sample rtm-sample-p
          create-attribute rename-attribute drop-attribute
          create-index drop-index rtm-index-p
          insert-into-table delete-selection
          .and. .or. .not.
          .==. .<=. .>=.
          ./=. .>>. .<<.
          .=><=. .=><<. .>><=. .>><<.
          .<<>>. .<<=>. .<=>>. .<==>.
          .min. .max. .pred. .succ. .floor. .ceiling.
          attr-value
          do-selection list-selection do-neighbors do-rows
          count-selection row-exists-p
          min-selection max-selection))

;;; -- * --
;;;
;;; RTM relation (base table) utilities:
;;;
  
(defmacro rtm-table-p (tbl-name)
  "Returns t if specified table exists, nil otherwise."
  `(if (get-table ,tbl-name) t nil))

(defmacro rtm-view-p (tbl-name)
  "Returns t if specified table is a view, nil otherwise."
  `(if (get-view ,tbl-name) t nil))

(defmacro rtm-sample-p (tbl-name)
  "Returns t if specified table is a sample, nil otherwise."
  `(if (get-sample ,tbl-name) t nil))

(defun table-attributes (tbl-name)
  "Returns a list of table attribute names for specified table."
  (let ((table (get-table tbl-name)))
    (when table
      (values (table-attr-name-list table)))))

(defun rtm-index-p (tbl-name attr-name)
  "Returns t if index exists for specified table and attribute, nil otherwise."
  (let ((ind-p nil))
    (if (and (rtm-table-p tbl-name) (push-table-scope tbl-name))
	(let ((attribute (current-attribute attr-name)))
	  (if attribute
	      (when (attribute-index attribute)
		(setf ind-p t))
	      (rtm-warning "Test for existence of index failed - invalid attribute \'" attr-name
			   " for table \'" tbl-name "."))
	  (pop-table-scope))
	(rtm-warning "Test for existence of index failed - table \'" tbl-name " doesn't exist."))
    (values ind-p)))

;;;
;;; RTM relation (base table) create and drop functions.
;;;

(defun create-table (tbl-name &key (attributes nil)
                              (initial-size *rtm-initial-table-size*))
  "Creates table and returns t if successful or nil if error.  Attributes are declared
as a list of attribute specifiers, where each specifier consists of (attribute-name
domain-name :key :index) where the optional keywords, when present, indicate
that the attribute is part of the key field, and/or should be indexed, respectively.
Domain names must have previously been declared, either by the system (built-in)
or the user.  (See documentation for create-domain for more information on 
built-in and user-defined domains.)"

  ;; Check for table too large error.
  (when (> (1+ initial-size) array-total-size-limit)
    (rtm-warning "Base table array too large for creation.")
    (return-from create-table (values nil)))  
  ;; Check for existing table error.
  (when (or (rtm-table-p tbl-name) (rtm-view-p tbl-name) (rtm-sample-p tbl-name))
    (rtm-warning "Table \'" tbl-name 
                 " can not be created - table, view, or sample already exists.")
    (return-from create-table (values nil)))
  ;; If no simple errors, create/initialize table structure and insert in system hash table.
  (let ((table (make-table
                :name tbl-name
                :attr-name-list nil
                :free-tuple-chain (init-free-tuple-chain initial-size)
                :degree 0
                :cardinality initial-size
                :tuple-count 0
                :indexed-attr-name-list nil
                :change-count 0)))
    (setf (get-table tbl-name) table)
    ;; For each attribute spec, create attribute for table
    (dolist (attr-spec attributes)
      (unless (create-attribute tbl-name attr-spec)
        (drop-table tbl-name)
        (rtm-warning "Table \'" tbl-name " can not be created - error creating attributes.")
        (return-from create-table (values nil))))
    ;; Reset the change count for new table.
    (setf (table-change-count table) 0)
    ;; Return success.
    (values t)))

(defun rename-table (old-tbl-name new-tbl-name)
  "Rename table, view, or sample from old-tbl-name to new-tbl-name."
  ;; Check for existing destination name error.
  (when (or (rtm-table-p new-tbl-name) (rtm-view-p new-tbl-name) (rtm-sample-p new-tbl-name))
    (rtm-warning "Table, view, or sample \'" old-tbl-name 
                 " can not be renamed to \'" new-tbl-name 
                 ", a table, view, or sample with that name already exists.")
    (return-from rename-table (values nil)))
  ;; No error, so continue.
  (cond ((rtm-table-p old-tbl-name)
         ;; Rename base table.
         (let ((table (get-table old-tbl-name)))
           ;; Update attribute system hash table.
           (dolist (attr-name (table-attr-name-list table))
             (let* ((old-tbl-attr-name (table-attribute-name old-tbl-name attr-name))
                    (new-tbl-attr-name (table-attribute-name new-tbl-name attr-name))
                    (attribute (get-attribute old-tbl-attr-name)))
               (setf (get-attribute new-tbl-attr-name) attribute)
               (rem-attribute old-tbl-attr-name)))
           ;; Update base table system hash table.
           (setf (table-name table) new-tbl-name)
           (setf (get-table new-tbl-name) table)
           (rem-table old-tbl-name)))
        ((rtm-view-p old-tbl-name)
         ;; Rename view.
         (progn (setf (get-view new-tbl-name) (get-view old-tbl-name))
                (rem-view old-tbl-name)))
        ((rtm-sample-p old-tbl-name)
         ;; Rename sample.
         (progn (setf (get-sample new-tbl-name) (get-sample old-tbl-name))
                (rem-sample old-tbl-name)))
        (t
         (rtm-warning "Table, view, or sample \'" old-tbl-name 
                      " can not be renamed - it does not exist.")
         (return-from rename-table (values nil))))
  (values t))

(defun drop-table (tbl-name)
  "Removes specified table from system.  Returns t is successful or nil if error."
  (let ((table (get-table tbl-name)))
    ;; Check for non-existing table.
    (unless table
      (rtm-warning "Table \'" tbl-name " can not be dropped - table doesn't exist.")
      (return-from drop-table (values nil)))
    ;; If no simple error, remove attribute and table structures.
    (dolist (attr-name (table-attr-name-list table))
      (drop-attribute tbl-name attr-name))
    (rem-table tbl-name))
    ;; Return success.
    (values t))

;;;
;;; RTM attribute creation and deletion functions.
;;;

(defun create-attribute (tbl-name attr-spec)
  "Creates a new attribute for table and returns t if successful or nil if error.  
The attribute to be created is declared by an attribute specifier, consisting of 
(attribute-name domain-name :key :index) where the optional keywords, when present, 
indicate that the attribute is part of the key field, and/or should be indexed, 
respectively. Domain names must have previously been declared, either by the system 
(built-in) or the user. (See documentation for create-domain for more information on 
built-in and user-defined domains.)"
  (let* ((table (get-table tbl-name))
         (attr-name (first attr-spec))
         (tbl-attr-name (table-attribute-name tbl-name attr-name)))
    ;; Check for undefined table error.
    (unless table
      (rtm-warning "Table \'" tbl-name ", attribute \'" attr-name 
                   " can not be created - table is a view or does not exist.")
      (return-from create-attribute (values nil)))
    ;; Check for duplicate attribute name.
    (when (get-attribute tbl-attr-name)
      (rtm-warning "Table \'" tbl-name ", attribute \'" attr-name 
                   " can not be created - duplicate attribute name.")
      (return-from create-attribute (values nil)))
    ;; Parse attribute spec
    (let* ((initial-size (table-cardinality table))
           (domain-name (second attr-spec))
           (key-p (and (member :key attr-spec) t))
           (rtm-index-p (and (member :index attr-spec) t))
           (domain (get-domain domain-name))
           (ordered-p nil))
      ;; Check for undefined domain error.
      (unless domain 
        (rtm-warning "Table \'" tbl-name ", attribute \'" attr-name 
                     " can not be created - domain \'" domain-name " is not defined.")
        (return-from create-attribute (values nil)))
      ;; Note if attribute is from ordered domain.
      (setf ordered-p (and (domain-equal-p domain) (domain-less-p domain)))
      ;; Check for invalid domain for use as part of key.
      (when (and key-p (not (domain-equal-p domain)))
        (rtm-warning "Table \'" tbl-name ", attribute \'" attr-name 
                     " can not be created - noncomparable domain \'" domain-name 
                     " can not be used in key.")
        (return-from create-attribute (values nil)))
      ;; Check for index on unordered domain error.
      (when (and rtm-index-p (not ordered-p))
        (rtm-warning "Table \'" tbl-name ", attribute \'" attr-name 
                     " can not be created - unordered domain \'" domain-name " can not be indexed.")
        (return-from create-attribute (values nil)))
      ;; Check for other attribute-spec syntax errors.
      (dolist (item (rest (rest attr-spec)))
        (unless (or (eql item :key) (eql item :index))
          (rtm-warning "Table \'" tbl-name ", attribute \'" attr-name 
                       " can not be created - invalid keyword \'" item
                       " in attribute-spec for \'" attr-name ".")
          (return-from create-attribute (values nil))))
      (when (< 4 (length attr-spec))
        (rtm-warning "Ignoring duplicate keyword items in attribute-spec while creating table \'" 
                     tbl-name ", attribute \'" attr-name "."))
      ;; Create data array for each attribute and put default domain value in element 0.
      (let* ((attribute (make-attribute
                         :name attr-name
                         :domain-name domain-name
                         :data nil
                         :index nil
                         :key-p key-p
                         :ordered-p ordered-p))
             (data-array (make-array (1+ initial-size)  
                                     :element-type (domain-lisp-data-type domain) 
                                     :adjustable t)))
        (setf (aref data-array 0) (domain-default-value domain)
              (attribute-data attribute) data-array)
        ;; Create index for indexed attributes.
        (when rtm-index-p
          (setf (attribute-index attribute)
                (index-init (+ initial-size initial-size))))
        ;; Insert attribute struct in system hash table.
        (setf (get-attribute tbl-attr-name) attribute)
        ;; Update table struct in system hash table.
        (push-end attr-name (table-attr-name-list table))
        (when rtm-index-p (push-end attr-name (table-indexed-attr-name-list table)))
        (incf (table-degree table))
        (incf (table-change-count table))
        ;; Return success.
        (values t)))))

(defun rename-attribute (tbl-name old-attr-name new-attr-name)
  "Rename attribute of base table tbl-name from old-attr-name to new-attr-name."
  (let* ((old-tbl-attr-name (table-attribute-name tbl-name old-attr-name))
         (new-tbl-attr-name (table-attribute-name tbl-name new-attr-name))
         (attribute (get-attribute old-tbl-attr-name)))
    ;; Check for non-existing table attribute error.
    (unless attribute
      (rtm-warning "Table \'" tbl-name ", attribute \'" old-attr-name 
                   " can not be renamed - it does not exist.")
      (return-from rename-attribute (values nil)))
    ;; No error, so rename attribute and update system hash tables.
    (let ((table (get-table tbl-name)))
      (setf (table-attr-name-list table)
            (substitute new-attr-name old-attr-name (table-attr-name-list table)))
      (when (attribute-index attribute)
        (setf (table-indexed-attr-name-list table)
              (substitute new-attr-name old-attr-name (table-indexed-attr-name-list table))))
      (setf (attribute-name attribute) new-attr-name)
      (setf (get-attribute new-tbl-attr-name) attribute)
      (rem-attribute old-tbl-attr-name)
      (values t))))

(defun drop-attribute (tbl-name attr-name)
  "Removes specified attribute from table.  Returns t is successful or nil if error."
  (let ((table (get-table tbl-name))
        (tbl-attr-name (table-attribute-name tbl-name attr-name)))
    ;; Check for non-existing table.
    (unless table
      (rtm-warning "Table \'" tbl-name ", attribute \'" attr-name 
                   " can not be dropped - table doesn't exist.")
      (return-from drop-attribute (values nil)))
    ;; If no simple error, remove attribute structure and update table struct.
    (rem-attribute tbl-attr-name)
    ;; Update table struct in system hash table.
    (setf (table-attr-name-list table) 
          (remove attr-name (table-attr-name-list table)))
    (setf (table-indexed-attr-name-list table) 
          (remove attr-name (table-indexed-attr-name-list table)))
    (decf (table-degree table))
    (incf (table-change-count table))    
    ;; Return success.
    (values t)))

;;;
;;; RTM relation (base table) tuple insertion and deletion functions.
;;;

(defun insert-into-table (tbl-name attr-value-list)
  "Inserts table row using specified attribute values or domain defaults,
updates necessary indexes, and returns t if successful or nil if error.
Attribute values are specified using property list syntax, that is, as a
sequence of pairs of attribute name followed by attribute value.
For example, (insert-into-table tbl-name (list a1-name a1-value
a2-name a2-value)). "
  (let ((table (get-table tbl-name))
	tuple-number)
    ;; Check for non-existing base table error.
    (unless table
      (rtm-warning "Insert of \'" attr-value-list
		   " into table \'" tbl-name " failed - table doesn't exist.")
      (return-from insert-into-table (values nil)))
    ;; Check for odd number of attribute names and values.
    (when (oddp (length attr-value-list))
      (rtm-warning "Insert of \'" attr-value-list
		   " into table \'" tbl-name " failed - attribute mismatch.")
      (return-from insert-into-table (values nil)))
    ;; If no simple errors, encode and verify attribute-values.
    (when (push-table-scope tbl-name)
      (let ((table (get-table tbl-name))
	    tuple-set)
	;; Check for invalid attribute name error.
	(do ((attr-list attr-value-list (rest (rest attr-list))))
	    ((null attr-list))
	  (let* ((attr-name (first attr-list))
		 (attribute (current-attribute attr-name)))
	    (unless attribute
	      (rtm-warning "Insert of \'" attr-value-list
			   " into table \'" tbl-name " failed - invalid attribute \'"
			   (first attr-list) " for table \'" tbl-name "." )
              (pop-table-scope)
	      (return-from insert-into-table (values nil)))))
        ;; Check for table size overflow error.
        (unless (setf tuple-number (table-tuple-allocate table))
          (rtm-warning "Insert of \'" attr-value-list
                       " into table \'" tbl-name " failed - table too large.")
          (pop-table-scope)
          (return-from insert-into-table (values nil)))
        ;; Iterate over all table attributes, inserting encoded data value or default,
	;;    and checking for key uniqueness where applicable.
	;;
	;;    Had to change member to member-plist in the let ext-attr-value
	;;    line.  Member would find a match if one of the values of an
	;;    attribute was the same as one of the attribute names. -AC 9/27/93
        (dolist (attr-name (table-attr-name-list table))
          (let* ((ext-attr-value (second (member-plist attr-name attr-value-list :test #'equal)))
                 (attribute (current-attribute attr-name))
                 (key-p (attribute-key-p attribute))
                 (domain-name (attribute-domain-name attribute))
                 (data-array (attribute-data attribute))
                 int-attr-value)
            ;; Get internal attribute value.
            (if ext-attr-value
		;; If provided, use specified attribute value.
		(setf int-attr-value (domain-encode-and-verify
                                      domain-name ext-attr-value))
		;; Otherwise, use domain default attribute value (stored in row 0).
		(setf int-attr-value (aref data-array 0)))
	    ;; If attribute is part of key, intersect into tuple-set those 
            ;;    tuple-numbers with matching values.
	    (when key-p
	      (if tuple-set
		  (setf tuple-set (tuple-set-intersect-set
				    tuple-set (query-eq attr-name int-attr-value)))
		  (setf tuple-set (query-eq attr-name int-attr-value))))
	    ;; Insert internal attribute value in newly allocated tuple.
	    (setf (aref data-array tuple-number) int-attr-value)))
	;; If tuple-set is not empty, then tuple has non-unique set of key attributes.
	(unless (or (null tuple-set) (tuple-set-empty-p tuple-set))
	  (rtm-warning "Insert of \'" attr-value-list
		       " into table \'" tbl-name " failed - non-unique key.")
	  (tuple-set-release tuple-set)
	  (table-tuple-release table tuple-number)
          (pop-table-scope)
	  (return-from insert-into-table (values nil)))
	(when tuple-set (tuple-set-release tuple-set))
	;; Tuple is OK, so mark tuple allocated, increment table tuple count, 
	;;   mark base table as changed, and update indices.
	(setf (next-free-tuple-pos (table-free-tuple-chain table) tuple-number) nil)
	(incf (table-tuple-count table))
	(table-mark-as-changed tbl-name)
	(dolist (attr-name (table-indexed-attr-name-list table))
          (let ((int-attr-value (aref (attribute-data (current-attribute attr-name)) 
                                                  tuple-number)))
            (index-insert attr-name int-attr-value tuple-number)))
	;; Restore old current table name and return success.
	(pop-table-scope)
	;; Return success.
	(values t)))))

;; Just like member, but only check the odd-positions for matches.
(defun member-plist (indicator plist &key (test #'eql))
  (cond
   ((or (null (car plist))
	(null (cdr plist)))
    nil)
   ((funcall test indicator (car plist))
    plist)
   (t
    (member-plist indicator (cdr (cdr plist)) :test test))))

(defmacro delete-selection (&key from (where nil))
  "Delete table rows that match specified where clause.  All rows will be
deleted if where clause is missing or nil.  Returns t if successful or nil if error."
  ;; Note:  Use of where clause requires a macro to set table name prior to
  ;;   evaluating where clause to produce tuple-set.
  (let ((result (gensym "RESULT")))
    `(let ((,result nil))
       (if (and (rtm-table-p ,from) (push-table-scope ,from))
	   (progn
	     (setf ,result (table-delete-tuples ,from (where-clause-tuple-set ,where)))
	     (pop-table-scope))
	   (rtm-warning "Delete failed - table \'" ,from " is a view or doesn't exist."))
       (values ,result))))

;;;
;;; RTM table-attribute index create and drop functions.
;;;

(defun create-index (tbl-name attr-name)
  "Creates a B-tree index on specified attribute of table.  
Returns t if successful or nil if error."
  (let ((table (get-table tbl-name)))
    ;; Check for non-existing table error.
    (unless table
      (rtm-warning "Create index on table \'" tbl-name " attribute \'"
		   attr-name " failed - table doesn't exist.")
      (return-from create-index (values nil)))
    (when (push-table-scope tbl-name)
      (let ((index-size (round (* 2 (* *rtm-table-expansion-factor*
					(table-tuple-count table)))))
            (attribute (current-attribute attr-name)))
	;; Check for non-existing attribute error.
	(unless attribute 
	  (rtm-warning "Create index failed - invalid attribute \'" attr-name 
		       " for table \'" tbl-name ".")
          (pop-table-scope)
	  (return-from create-index (values nil)))
        ;; Check for existing index error.
        (when (attribute-index attribute)
          (rtm-warning "Index can not be created for attribute \'" attr-name
                       " in table \'" tbl-name " - index already exists.")
          (pop-table-scope)
          (return-from create-index (values nil)))
        ;; Check for un-ordered domain error.
        (unless (attribute-ordered-p attribute)
          (rtm-warning "Index can not be created for attribute \'" attr-name
                       " in table \'" tbl-name " - unordered domain \'"
                       (attribute-domain-name attribute) ".")
          (pop-table-scope)
          (return-from create-index (values nil)))
        ;; No errors, so create and initialize index and update attribute structure.
	(let ((data-array (attribute-data attribute))
              (tuple-set (query-all)))
	  ;; Create index, update indexed column list, and intialize index.
	  (setf (attribute-index attribute) (index-init index-size))
	  (push attr-name (table-indexed-attr-name-list table))
	  (do-tuple-set (tuple-number tuple-set)
	    (index-insert
	      attr-name (aref data-array tuple-number) tuple-number))
	  ;; Release tuple set storage.
	  (tuple-set-release tuple-set))
	;; Restore previous table as current.
	(pop-table-scope)))
    (values t)))  

(defun drop-index (tbl-name attr-name)
  "Removes the B-tree index on specified attribute of table.  
Returns t if successful or nil if error."
  (let ((table (get-table tbl-name)))
    ;; Check for non-existing table.
    (unless table
      (rtm-warning "Create index on table \'" tbl-name " attribute \'"
		   attr-name " failed - table doesn't exist.")
      (return-from drop-index (values nil)))
    (when (push-table-scope tbl-name)
      (let ((attribute (current-attribute attr-name)))
	;; Check for non-existing attribute error.
	(unless attribute 
	  (rtm-warning "Create index failed - invalid attribute \'" attr-name 
		       " for table \'" tbl-name ".")
          (pop-table-scope)
	  (return-from drop-index (values nil)))
        ;; Check for non-existing index error.
        (unless (attribute-index attribute)
          (rtm-warning "Index can not be dropped for attribute \'" attr-name
                       " in table \'" tbl-name " - index doesn't exist.")
          (pop-table-scope)
          (return-from drop-index (values nil)))
        ;; No errors, so remove index from attribute and attr-name from list in table structure.
        (let ((old-indexed-attr-names (table-indexed-attr-name-list table))
              (new-indexed-attr-names nil))
	  ;; Update indexed attribute name list and drop index.
	  (dolist (indexed-attr-name old-indexed-attr-names)
	    (unless (eql indexed-attr-name attr-name)
	      (push indexed-attr-name new-indexed-attr-names))) 
	  (setf (table-indexed-attr-name-list table) new-indexed-attr-names)
	  (setf (attribute-index attribute) nil)))
      ;; Restore previous table as current.
      (pop-table-scope))
    (values t)))

;;;
;;; Structured Query Selection Macros:
;;;
;;;   Single Attribute Comparison (theta clause) Operators:
;;;

(defmacro .==. (attr-name attr-value)
  "Selects rows where table attribute is equal to query attribute."
  `(query-eq ,attr-name ,attr-value))

(defmacro ./=. (attr-name attr-value)
  "Selects rows where table attribute is not equal to query attribute."
  `(query-ne ,attr-name ,attr-value))

(defmacro .<<. (attr-name attr-value)
  "Selects rows where table attribute is less than query attribute."
  `(query-lt ,attr-name ,attr-value))

(defmacro .>=. (attr-name attr-value)
  "Selects rows where table attribute is greater than or equal to query attribute."
  `(query-ge ,attr-name ,attr-value))

(defmacro .>>. (attr-name attr-value)
  "Selects rows where table attribute is greater than query attribute."
  `(query-gt ,attr-name ,attr-value))

(defmacro .<=. (attr-name attr-value)
  "Selects rows where table attribute is less than or equal to query attribute."
  `(query-le ,attr-name ,attr-value))

;;;
;;;   Double Attribute Comparison (between range) Operators:
;;;

(defmacro .=><=. (attr-name attr-value-1 attr-value-2)
  "Selects rows where table attribute is greater than or equal to first 
query attribute and less than or equal to second query attribute."
  `(query-ge-le ,attr-name ,attr-value-1 ,attr-value-2))

(defmacro .>><=. (attr-name attr-value-1 attr-value-2)
  "Selects rows where table attribute is greater than first query attribute
and less than or equal to second query attribute."
  `(query-gt-le ,attr-name ,attr-value-1 ,attr-value-2))

(defmacro .=><<. (attr-name attr-value-1 attr-value-2)
  "Selects rows where table attribute is greater than or equal to first 
query attribute and less than second query attribute."
  `(query-ge-lt ,attr-name ,attr-value-1 ,attr-value-2))

(defmacro .>><<. (attr-name attr-value-1 attr-value-2)
  "Selects rows where table attribute is greater than first query attribute
and less than second query attribute."
  `(query-gt-lt ,attr-name ,attr-value-1 ,attr-value-2))

;;;
;;;   Double Attribute Comparison (not between range) Operators:
;;;

(defmacro .<==>. (attr-name attr-value-1 attr-value-2)
  "Selects rows where table attribute is less than or equal to first 
query attribute and greater than or equal to second query attribute."
  `(query-le-ge ,attr-name ,attr-value-1 ,attr-value-2))

(defmacro .<<=>. (attr-name attr-value-1 attr-value-2)
  "Selects rows where table attribute is less than first query attribute
and greater than or equal to second query attribute."
  `(query-lt-ge ,attr-name ,attr-value-1 ,attr-value-2))

(defmacro .<=>>. (attr-name attr-value-1 attr-value-2)
  "Selects rows where table attribute is less than or equal to first 
query attribute and greater than second query attribute."
  `(query-le-gt ,attr-name ,attr-value-1 ,attr-value-2))

(defmacro .<<>>. (attr-name attr-value-1 attr-value-2)
  "Selects rows where table attribute is less than first query attribute
and greater than second query attribute."
  `(query-lt-gt ,attr-name ,attr-value-1 ,attr-value-2))

;;;
;;;   Miscellaneous Special Operators:
;;;

(defmacro .max. (attr-name)
  "Selects rows where table attribute is the maximum value in table."
  `(query-max ,attr-name))

(defmacro .min. (attr-name)
  "Selects rows where table attribute is the minimum value in table."
  `(query-min ,attr-name))

(defmacro .pred. (attr-name attr-value)
  "Selects rows where table attribute has greatest value less than query attribute."
  `(query-pred ,attr-name ,attr-value))

(defmacro .succ. (attr-name attr-value)
  "Selects rows where table attribute has smallest value greater than query attribute."
  `(query-succ ,attr-name ,attr-value))

(defmacro .floor. (attr-name attr-value)
  "Selects rows where table attribute has greatest value less than or equal to query attribute."
  `(query-floor ,attr-name ,attr-value))

(defmacro .ceiling. (attr-name attr-value)
  "Selects rows where table attribute has smallest value greater than or equal to query attribute."
  `(query-ceiling ,attr-name ,attr-value))

;;;
;;;   Logic Operators:
;;;

(defmacro .and. (arg1 &rest args)
  "Selects rows matching every argument's selection criteria."
  (if args
      (let ((smallest-tuple-set (gensym "SMALLEST"))
	    (smallest-size (gensym "MIN-SIZE"))
	    (candidate-tuple-set (gensym "CANDIDATE"))
	    (candidate-size (gensym "SIZE"))
	    (tuple-set-list (gensym "ARG-LIST"))
	    (arg (gensym "ARG")))
	;; Macro expand and evaluate each clause once, while searching for smallest tuple set.
	;; This produces the smallest tuple-set and a list of tuple-sets to be intersected with it.
	`(let* ((,smallest-tuple-set ,arg1)
		(,smallest-size (tuple-set-cardinality ,smallest-tuple-set))
		(,tuple-set-list nil))
	   ,@(mapcar #'(lambda (arg)
			 `(let* ((,candidate-tuple-set ,arg)
				 (,candidate-size (tuple-set-cardinality
						    ,candidate-tuple-set)))
			    (if (< ,candidate-size ,smallest-size)
				(setf ,tuple-set-list (cons ,smallest-tuple-set
							    ,tuple-set-list)
				      ,smallest-tuple-set ,candidate-tuple-set
				      ,smallest-size ,candidate-size)
				(setf ,tuple-set-list (cons ,candidate-tuple-set
							    ,tuple-set-list)))))
		     args)
	   ;; Iterate over tuple-set list, intersecting into the smallest.
	   ;; Return if smallest becomes empty.
	   (unless (tuple-set-empty-p ,smallest-tuple-set)
	     (dolist (,arg ,tuple-set-list)
	       (setf ,smallest-tuple-set
		     (tuple-set-intersect-set-fast ,smallest-tuple-set ,arg))
	       (when (tuple-set-empty-p ,smallest-tuple-set)
		 (return))))
	   ,smallest-tuple-set))
      (progn
	(rtm-warning "at least 2 arguments are needed for .and.")
	arg1)))

(defmacro .or. (arg1 &rest args)
  "Selects rows matching at least one argument's selection criteria."
  (if args
      (let ((largest-tuple-set (gensym "LARGEST"))
	    (largest-size (gensym "MAX-SIZE"))
	    (candidate-tuple-set (gensym "CANDIDATE"))
	    (candidate-size (gensym "SIZE"))
	    (tuple-set-list (gensym "ARG-LIST"))
	    (arg (gensym "ARG")))
	;; Macro expand and evaluate each clause once, while searching for largest tuple set.
	;; This produces the largest tuple-set and a list of tuple-sets to be unioned with it.
	`(let* ((,largest-tuple-set ,arg1)
		(,largest-size (tuple-set-cardinality ,largest-tuple-set))
		(,tuple-set-list nil))
	   ,@(mapcar #'(lambda (arg)
			 `(let* ((,candidate-tuple-set ,arg)
				 (,candidate-size (tuple-set-cardinality
						    ,candidate-tuple-set)))
			    (if (> ,candidate-size ,largest-size)
				(setf ,tuple-set-list (cons ,largest-tuple-set
							    ,tuple-set-list)
				      ,largest-tuple-set ,candidate-tuple-set
				      ,largest-size ,candidate-size)
				(setf ,tuple-set-list (cons ,candidate-tuple-set
							    ,tuple-set-list)))))
		     args)
	   ;; Iterate over tuple-set list, unioning into the largest.
	   (unless (tuple-set-empty-p ,largest-tuple-set)
	     (dolist (,arg ,tuple-set-list)
	       (unless (tuple-set-empty-p ,arg)
		 (setf ,largest-tuple-set
		       (tuple-set-union-set-fast ,largest-tuple-set ,arg)))))
	   ,largest-tuple-set))
      (progn
	(rtm-warning "at least 2 arguments are needed for .or.")
	arg1)))

(defmacro .not. (arg)
  "Selects rows not matching argument's selection criteria."
  (let ((op (first arg))
	(tail (rest arg)))
    (case op
      (.not. `(values ,@tail))
      (.and. `(.or. ,@(mapcar #'(lambda (sub-arg) `(.not. ,sub-arg)) tail)))
      (.or. `(.and. ,@(mapcar #'(lambda (sub-arg) `(.not. ,sub-arg)) tail)))
      (.==. `(./=. ,@tail))
      (./=. `(.==. ,@tail))
      (.<<. `(.>=. ,@tail))
      (.>=. `(.<<. ,@tail))
      (.>>. `(.<=. ,@tail))
      (.<=. `(.>>. ,@tail))
      (.=><=. `(.<<>>. ,@tail))
      (.>><=. `(.<=>>. ,@tail))
      (.=><<. `(.<<=>. ,@tail))
      (.>><<. `(.<==>. ,@tail))
      (.<==>. `(.>><<. ,@tail))
      (.<<=>. `(.=><<. ,@tail))
      (.<=>>. `(.>><=. ,@tail))
      (.<<>>. `(.=><=. ,@tail))
      (.max. `(./=. ,@tail (.max. ,@tail)))
      (.min. `(./=. ,@tail (.min. ,@tail)))
      (.pred. `(./=. ,(first tail) (.pred. ,@tail)))
      (.succ. `(./=. ,(first tail) (.succ. ,@tail)))
      (.floor. `(./=. ,(first tail) (.floor. ,@tail)))
      (.ceiling. `(./=. ,(first tail) (.ceiling. ,@tail)))
      (otherwise (rtm-error "Invalid argument \'" op " for .not. operator.")))))

;;; Attribute accessor functions are invalid outside of querys.

(defmacro attr-value (attr-name)
  "Accesses table attribute values in selection context.  Only valid 
within the lexical scope of a do-selection or do-neighbors form.  To update
table attribute values, use setf of this form."
  (declare (ignore attr-name))
  (rtm-error "attr-value only valid within the lexical scope of"
	     "a do-selection or do-neighbors form."))

(defun evaluate-where-clause (where-clause)
  "Evaluate where clause if necessary, check for errors,
and return tuple-set if successful or nil if error."
  (let (tuple-set)
    (if (or (null where-clause) (typep where-clause 'tuple-set))
	(setf tuple-set where-clause)
	(progn
	  (setf tuple-set (eval where-clause))
	  (unless (typep tuple-set 'tuple-set)
	    (rtm-warning "Invalid where-clause \'" where-clause "specified - matching all rows in table.")
	    (setf tuple-set nil))))
    (values tuple-set)))

(defun where-clause-tuple-set (where-clause)
  "Return the tuple-set for the current table and query.  If current table is a view,
make sure view-tuple-set is current, and intersect it with specified query."
  (let ((view (current-scope-view-or-sample))
	(tuple-set (evaluate-where-clause where-clause)))
    (if (view-p view)
	;; If query is of a view, make sure stored tuple-list is up-to-date,
	;;   and intersect it with specified where-clause.
	(let ((prev-change-count (view-change-count view))
	      (curr-change-count (table-change-count
				   (get-table (current-table-name)))))
	  (if (= prev-change-count curr-change-count)
	      ;; Use stored list of tuple-numbers for view.
	      (if tuple-set 
		  (setf tuple-set (tuple-set-intersect-list tuple-set 
				    (view-tuple-list view)))
		  (setf tuple-set (tuple-set-union-list (tuple-set-allocate)
				    (view-tuple-list view))))
	      ;; Re-compute and update stored list of tuple-numbers for view.
	      (let ((view-tuple-set (evaluate-where-clause
				      (view-where-clause view)))) 
		(setf (view-tuple-list view) (tuple-set-list view-tuple-set) 
		      (view-change-count view) curr-change-count)
		(if tuple-set
		    ;; Partial selection from a view intersects two tuple-sets.
		    (setf tuple-set (tuple-set-intersect-set tuple-set view-tuple-set))
		    ;; Total selection from a view simply uses view-tuple-set.
		    (setf tuple-set view-tuple-set)))))
	;; if query of a base table, use tuple-set produced above, or query-all if error occurred.
	(setf tuple-set (or tuple-set (query-all))))
    (values tuple-set)))

(defmacro do-selection ((&key (from nil) (where nil) (distinct nil) (order-by nil)) 
                        &body body)
  "Iterates over selected table rows.  Projection of attributes (access to values)
is achieved through the use of (attr-value attr-name) from within body of form.
Attribute values may be modified using setf on this form.  Returns value of last 
form executed in loop or nil if error."
  (let ((tuple-set (gensym "TUPLE-SET"))
	(sorted-tuple-array (gensym "TUPLE-SEQ"))
	(sequence-index (gensym "SEQ-INDEX"))
	(tuple-number (gensym "ROW"))
	(value (gensym "VALUE")))
    `(macrolet ((attr-value (attr-name)
		  `(table-element-access ,',tuple-number ,attr-name)))
       (let ((,value nil))
	 ;; Push table scope and check for table-name errors.
	 (when (push-table-scope ,from)
	   (let ((,tuple-set (where-clause-tuple-set ,where)))
             ;; Remove duplicates if distinct selection requested.
             ,(if distinct
                `(when ,distinct
                   (setf ,tuple-set (distinct-tuple-set ,tuple-set ,distinct))))
	     ;; Sort selected tuples if requested, and iterate through them.
	     ,(if order-by
		  `(if ,order-by
		       (setf ,value
			     (let ((,sorted-tuple-array (sort-tuples ,tuple-set ,order-by))
				   ,tuple-number)
			       (dotimes (,sequence-index (array-total-size ,sorted-tuple-array))
				 (setf ,tuple-number (aref ,sorted-tuple-array ,sequence-index))
				 ,@body)))
		       (setf ,value
			     (do-tuple-set (,tuple-number ,tuple-set)
			       ,@body)))
		  `(setf ,value
			 (do-tuple-set (,tuple-number ,tuple-set)
			   ,@body)))
	     ;; Release tuple-set storage and pop table scope.
	     (tuple-set-release ,tuple-set)
	     (pop-table-scope)))
	 ;; Return value of body.
	 (values ,value)))))

;;;
;;; Functions for creating and dropping views:
;;;

;;
;; Literal where-clause parsing to substitute values for variables.
;;   This is used to cache the query spec for a view so that it
;;   can be re-evaluated when the base table has changed.
;;

#+GARBAGE
(defmacro literal-where-clause (where)
  "Return where clause with attribute name and value variables evaluated."
  (let* ((op (first where))
	 (old-args (rest where)))
    (if (member op '(.and. .or. .not.) :test #'equal)
	(let ((new-args nil))
	  (dolist (arg old-args)
	    (push-end (macroexpand `(literal-where-clause ,arg)) new-args))
	  `(list ,(list 'quote op) ,@new-args))
	`(literal-theta ,where))))

#+GARBAGE
(defmacro literal-theta (theta-clause)
  `(list ,(list 'quote (first theta-clause))
	 ,@(mapcar #'(lambda (arg)
		       `(list 'quote ,arg))
		   (rest theta-clause))))

(defmacro create-view (view-name &key (from nil) (where nil))
  "Create a view named view-name using specified :from table and :where clause query."
  ;; Check for compile-time errors.
  (cond
   ((null from)
    (rtm-error "Create view failed - invalid value \'" from
               " specified for :from keyword.")
    (values nil))
   (t
    (let ((where-clause (gensym "WHERE"))
          (view (gensym "VIEW")))
      `(if (or (rtm-table-p ,view-name) (rtm-view-p ,view-name) (rtm-sample-p ,view-name))
         (rtm-warning "View \'" ,view-name 
                      " can not be created - table, view, or sample already exists.")
         (when (push-table-scope ,from)
           (let ((,where-clause ,where)
                 (,view (current-scope-view-or-sample)))
             (when ,view
               (setf ,where-clause
                     (list '.and.
                           (view-where-clause ,view)
                           ,where-clause)))
             (setf (get-view ,view-name)
                   (make-view
                    :base-table-name (current-table-name)
                    :where-clause ,where-clause)))
           (pop-table-scope)))))))

(defun drop-view (view-name)
  "Drop the view named view-name."
  (unless (rtm-view-p view-name)
    (rtm-warning "View \'" view-name " can not be dropped - view doesn't exist.")
    (return-from drop-view (values nil)))
  (rem-view view-name)
  (values t))

;;
;; Functions for creating and dropping samples from tables or views:
;;

(defun random-sample (population-array sample-size with-replacement-p)
  "Create an array of fixnums containing sample-size tuple-numbers drawn from 
population array, allowing replacement if requested.  Returns array if successful
or nil if error.  Note: population array is assumed to contain non-zero 
tuple-numbers, and is destructively modified sample is drawn without replacement."
  (let ((n (array-total-size population-array))
        (k sample-size))
    ;; Check for size error:
    (when (and (not with-replacement-p) (> k n))
      (rtm-warning "Sample size can not be greater than population size without replacement.")
      (return-from random-sample (values nil)))
    (let ((sample-array (make-array k :element-type 'fixnum)))
      (if (and (not with-replacement-p) (= n k))
        ;; Special case where each is selected once, simply copy array
        (dotimes (i n)
          (setf (aref sample-array i) (aref population-array i)))
        ;; Otherwise, draw random sample of size k, with replacement if specified.
        (dotimes (i k)
          (if with-replacement-p
            (setf (aref sample-array i) (aref population-array (random n)))
            (loop
              (let* ((j (random n))
                     (tuple-number (aref population-array j)))
                (unless (= 0 tuple-number)
                  (setf (aref sample-array i) tuple-number)
                  (setf (aref population-array j) 0)
                  (return)))))))
      (values sample-array))))

(defmacro create-sample (sample-name &key (from nil) (where nil) 
                                     (sample-size nil) (with-replacement-p nil))
  "Create a named sample by random selection from the specified table or view.
Samples (as well as tables and views) are iterated over using the do-rows form."
  (let ((tuple-set (gensym "TUPLE-SET"))
        (population-array (gensym "POPULATION"))
        (sample-array (gensym "SAMPLE")))
    `(if (or (rtm-table-p ,sample-name) (rtm-view-p ,sample-name) (rtm-sample-p ,sample-name))
       (rtm-warning "Sample \'" ,sample-name 
                    " can not be created - table, view, or sample already exists.")
       ;; Push table scope and check for table-name errors.
       (when (push-table-scope ,from)
         (let* ((,tuple-set (where-clause-tuple-set ,where))
                (,population-array (tuple-set-array ,tuple-set))
                (,sample-array (random-sample ,population-array ,sample-size ,with-replacement-p)))
           ;; if successful sampling, create sample structure and update system hash table.
           (if ,sample-array
             (setf (get-sample ,sample-name)
                   (make-sample
                    :base-table-name (current-table-name)
                    :tuple-array ,sample-array))
             (rtm-warning "Sample \'" ,sample-name " can not be created because of a size error."))
           ;; Release tuple-set storage and pop table scope.
           (tuple-set-release ,tuple-set)
           (pop-table-scope)))
       ;; Return success or error indicator.
       (values (if ,sample-array t nil)))))

(defun drop-sample (sample-name)
  "Drop the sample named sample-name."
  (unless (rtm-sample-p sample-name)
    (rtm-warning "Sample \'" sample-name " can not be dropped - sample doesn't exist.")
    (return-from drop-sample (values nil)))
  (rem-sample sample-name)
  (values t))
    
(defmacro do-neighbors ((distance &key
				    (from-point nil)
				    (in-table nil)
				    (distance-metric nil))
			  &body body)
  "Iterates over table rows that are neighbors of from-point in n-dimensional minkowski
metric space over numeric domains.  Distance is measured according to the specified
distance-metric, which must be one of the following: :city-block, :euclidean (default),
or :euclidean-squared.  Distance is a variable that will be bound within body to the
distance between the query point and the current selected neighbor.  In-table is the query
table, and from-point is the query point given as a property list of attribute value pairs.
The loop body should contain an explicit return unless every neighbor (i.e. the whole table)
is desired in the query selection.  Attr-value forms may be used to access table attribute
values of the current row.  Returns value of last form executed in loop or nil if error."
  ;; Check for compile-time errors.
  (cond
    ((null from-point)
     (rtm-error "Do neighbors failed - invalid value \'" from-point
		" specified for :from-point keyword.")
     (values nil))
    ((null in-table)
     (rtm-error "Do neighbors failed - invalid value \'" in-table
		" specified for :in-table keyword.")
     (values nil))
    (t
     (let ((table (gensym "TABLE"))
	   (tuple-number (gensym "TUPLE-NUMBER"))
	   (neighbor (gensym "NEIGHBOR"))
	   (nn-rec (gensym "NN-REC"))
	   (value (gensym "VALUE")))
       ;; Check for run-time errors.
       `(cond
	  ((or (null ,from-point)
	       (not (listp ,from-point))
	       (oddp (length ,from-point)))
	   (rtm-warning "Do neighbors failed - invalid value \'" ,from-point
			" specified for :from-point keyword.")
	   (values nil))
	  ((not (rtm-table-p ,in-table))
	   (rtm-warning "Do neighbors failed - table \'" ,in-table " is a view or doesn't exist.")
	   (values nil))
	  (t
	   (macrolet ((attr-value (attr-name)
			`(table-element-access ,',tuple-number ,attr-name)))
	     (let ((,value nil))
	       (when (push-table-scope ,in-table)
		 (let* ((,table (get-table ,in-table))
			,nn-rec ,neighbor ,tuple-number ,distance)
		   (when (> (table-tuple-count ,table) 0)
		     ;; Initialize nearest neighbor data structures.
		     (when (setf ,nn-rec (nn-init ,from-point ,distance-metric))
		       (setf ,value
			     (loop
			       (setf ,neighbor (nn-next ,nn-rec))
			       (unless ,neighbor (return))
			       (setf ,tuple-number (neighbor-tuple-number ,neighbor))
			       (setf ,distance (neighbor-distance ,neighbor))
			       ,@body))
		       ;; Release candidates tuple-set and priority-queue working storage.
		       (tuple-set-release (nn-candidates ,nn-rec))
		       (priority-queue-release (nn-priority-queue ,nn-rec)))
		     ;; Restore previous table name.
		     (pop-table-scope))))
	       ;; Return value of body.	 
	       (values ,value)))))))))

(defmacro list-selection (&key (attrs nil) from (where nil) (distinct nil) (order-by nil))
  "Returns a list of values if attrs is a single attr-name, or a list of lists (rows
of projected attribute values) if attrs is a list of attribute names."
  ;; Check for compile-time errors.
  (cond
    ((null from)
     (rtm-error "List selection failed - invalid value \'" from
		" specified for :from keyword.")
     (values nil))
    (t
     (let ((tuple-list (gensym "TUPLE-LIST"))
	   (attr-list (gensym "ATTR-LIST"))
	   (tuple (gensym "TUPLE")))
       `(let ((,tuple-list nil))
	  (if (push-table-scope ,from)
	      (progn
		(when (table-attributes-p (current-table-name) ,attrs)
		  (if (and ,attrs (symbolp ,attrs))
		      ;; If only a single attribute is specified, return list of values.
		      (do-selection (:from ,from :where ,where 
                                           :distinct ,distinct :order-by ,order-by)
			(push (attr-value ,attrs) ,tuple-list))
		      ;; Otherwise, return list of tuples containing projected attributes.
                      ;;    If no attrs specified, use all table attribute names.
		      (let ((,attr-list (reverse (or ,attrs (table-attr-name-list (current-table))))))
                        (do-selection (:from ,from :where ,where 
                                             :distinct ,distinct :order-by ,order-by)
			  (let (,tuple)
			    (dolist (attr ,attr-list)
			      (push (attr-value attr) ,tuple))
			    (push ,tuple ,tuple-list))))))
		(pop-table-scope))
	      (rtm-warning "List selection failed - table or view \'" ,from " doesn't exist."))
	  (values (nreverse ,tuple-list)))))))

(defmacro count-selection (&key from (where nil) (distinct nil))
  "Returns count of number of table rows that match selection criteria."
  ;; Check for compile-time errors.
  (cond
    ((null from)
     (rtm-error "Count selection failed - invalid value \'" from
		" specified for :from keyword.")
     (values nil))
    (t
     (let ((value (gensym "VALUE"))
	   (tuple-set (gensym "TUPLE-SET")))
       ;; Check for run-time errors (warnings).
       `(let ((,value nil))
	  (if (or (rtm-table-p ,from) (rtm-view-p ,from))
	      ,(if where
		   ;; Where clause specified at compile time.
		   `(when (push-table-scope ,from)
		      (let ((,tuple-set (where-clause-tuple-set ,where)))
                        ;; Remove duplicates if distinct selection requested.
                        ,(if distinct
                           `(when ,distinct
                              (setf ,tuple-set (distinct-tuple-set ,tuple-set ,distinct))))
			;; Return number of elements in tuple-set.
			(setf ,value (tuple-set-cardinality ,tuple-set))
			;; Release tuple-set storage and restore previous table name.
			(tuple-set-release ,tuple-set))
		      (pop-table-scope))
		   ;; Null where clause specified at compile time.
		   `(cond ((and (rtm-table-p ,from) (null ,distinct))
			   (setf ,value (table-tuple-count (get-table ,from))))
			  ((push-table-scope ,from)
			   (let ((,tuple-set (where-clause-tuple-set ,where)))
                             ;; Remove duplicates if distinct selection requested.
                             ,(if distinct
                                `(when ,distinct
                                   (setf ,tuple-set (distinct-tuple-set ,tuple-set ,distinct))))
                             ;; Return number of elements in tuple-set.
			     (setf ,value (tuple-set-cardinality ,tuple-set))
			     ;; Release tuple-set storage and restore previous table name.
			     (tuple-set-release ,tuple-set))
			   (pop-table-scope))))
	      (rtm-warning "Count selection failed - table or view \'" ,from " doesn't exist."))
	  (values ,value))))))

(defmacro row-exists-p (&key in-table (where nil))
  "Returns t if any table row satisfies specified selection criteria, nil otherwise."
  `(< 0 (count-selection :from ,in-table :where ,where)))

(defmacro max-selection (&key attr from (where nil))
  "Returns the maximum value of specified attribute over rows matching selection criteria."
  ;; Check for compile-time errors.
  (cond
    ((null attr)
     (rtm-error "Max selection failed - invalid value \'" attr
		" specified for :attr keyword.")
     (values nil))
    ((null from)
     (rtm-error "Max selection failed - invalid value \'" from
		" specified for :from keyword.")
     (values nil))
    (t
     (let ((domain (gensym "DOMAIN"))
	   (equal-p-fn (gensym "EQ-FN"))
	   (less-p-fn (gensym "LT-FN"))
	   (max-value (gensym "MAX-VALUE")))
       ;; Check for run-time errors or warnings.
       `(cond
	  ((null ,attr)
	   (rtm-warning "Max selection failed - invalid value \'" ,attr
			" specified for :attr keyword.")
	   (values nil))
	  ((not (or (rtm-table-p ,from) (rtm-view-p ,from)))
	   (rtm-warning "Max selection failed - table or view \'" ,from " doesn't exist.")
	   (values nil))
	  (t
	   (let (,max-value)
	     ,(if where
		  ;; If a where clause is specified, iterate over selected tuples.
		  `(when (push-table-scope ,from)
		     (let* ((,domain (current-domain ,attr))
			    (,equal-p-fn (domain-equal-p ,domain))
			    (,less-p-fn (domain-less-p ,domain)))
		       (do-selection (:from ,from :where ,where)
			 (when (or (null ,max-value)
				   (attr-gt-p (attr-value ,attr) ,max-value
					      ,equal-p-fn ,less-p-fn))
			   (setf ,max-value (attr-value ,attr)))))
		     (pop-table-scope))
		  ;; Otherwise, if base table, use .max. comparison operator for max of whole table.
		  `(cond ((rtm-table-p ,from)
			  (do-selection (:from ,from :where (.max. ,attr))
			    (setf ,max-value (attr-value ,attr))
			    (return)))
			 ((push-table-scope ,from)
			  (let* ((,domain (current-domain ,attr))
                                 (,equal-p-fn (domain-equal-p ,domain))
                                 (,less-p-fn (domain-less-p ,domain)))
			    (do-selection (:from ,from :where ,where)
			      (when (or (null ,max-value)
					(attr-gt-p (attr-value ,attr) ,max-value
						   ,equal-p-fn ,less-p-fn))
				(setf ,max-value (attr-value ,attr)))))
			  (pop-table-scope))))
	     (values ,max-value))))))))

(defmacro min-selection (&key attr from (where nil))
  "Returns the minimum value of specified attribute over rows matching selection criteria."
  ;; Check for compile-time errors.
  (cond
    ((null attr)
     (rtm-error "Min selection failed - invalid value \'" attr
		" specified for :attr keyword.")
     (values nil))
    ((null from)
     (rtm-error "Min selection failed - invalid value \'" from
		" specified for :from keyword.")
     (values nil))
    (t
     (let ((domain (gensym "DOMAIN"))
	   (equal-p-fn (gensym "EQ-FN"))
	   (less-p-fn (gensym "LT-FN"))
	   (min-value (gensym "MIN-VALUE")))
       ;; Check for run-time errors (warnings).
       `(cond
	  ((null ,attr)
	   (rtm-warning "Min selection failed - invalid value \'" ,attr
			" specified for :attr keyword.")
	   (values nil))
	  ((not (or (rtm-table-p ,from) (rtm-view-p ,from)))
	   (rtm-warning "Min selection failed - table \'" ,from " doesn't exist.")
	   (values nil))
	  (t
	   (let (,min-value)
	     ,(if where
		  ;; If a where clause is specified, iterate over selected tuples.
		  `(when (push-table-scope ,from)
		     (let* ((,domain (current-domain ,attr))
                            (,equal-p-fn (domain-equal-p ,domain))
                            (,less-p-fn (domain-less-p ,domain)))
		       (do-selection (:from ,from :where ,where)
			 (when (or (null ,min-value)
				   (funcall ,less-p-fn (attr-value ,attr) ,min-value))
			   (setf ,min-value (attr-value ,attr)))))
		     (pop-table-scope))
		  ;; Otherwise, if base table, use .min. comparison operator for min of whole table.
		  `(cond ((rtm-table-p ,from)
			  (do-selection (:from ,from :where (.min. ,attr))
			    (setf ,min-value (attr-value ,attr))
			    (return)))
			 ((push-table-scope ,from)
			  (let* ((,domain (current-domain ,attr))
                                 (,less-p-fn (domain-less-p ,domain)))
			    (do-selection (:from ,from :where ,where)
			      (when (or (null ,min-value)
					(funcall ,less-p-fn (attr-value ,attr) ,min-value))
				(setf ,min-value (attr-value ,attr)))))
			  (pop-table-scope))))
	     ,min-value)))))))

;;
;; Iterator form for table, view, or sample:
;;

(defun current-tuple-array ()
  "Create and return an array containing all the tuple-numbers 
for the current table, view, or sample."
  (let ((tuple-array nil)
        (view-or-sample (current-scope-view-or-sample)))
    (cond ((sample-p view-or-sample)
           (setf tuple-array (sample-tuple-array view-or-sample)))
          (t
           (let ((tuple-set (query-all)))
             (setf tuple-array (tuple-set-array tuple-set))
             (tuple-set-release tuple-set))))
    (values tuple-array)))

(defmacro do-rows ((&key (from nil) (order-by nil)) 
                   &body body)
  "Iterates over all rows of specified table, view, or sample.  Projection of 
attributes (access to values) is achieved through the use of (attr-value attr-name) 
from within body of form.  Attribute values may be modified using setf on this form,
unless iteration is over a sample.  Returns value of last form executed in loop or 
nil if error."
  (let ((tuple-array (gensym "TUPLES"))
	(sequence-index (gensym "SEQ-INDEX"))
	(tuple-number (gensym "ROW"))
	(value (gensym "VALUE")))
    `(macrolet ((attr-value (attr-name)
		  `(table-element-access ,',tuple-number ,attr-name)))
       (let ((,value nil))
	 ;; Push table scope (allow samples) and check for table-name errors.
	 (when (push-table-scope ,from t)
	   (let ((,tuple-array (current-tuple-array)))
	     ;; Sort tuples if requested, and iterate through them.
	     ,(when order-by
                `(when ,order-by
                   (setf ,tuple-array (sort-tuples ,tuple-array ,order-by))))
             (setf ,value
                   (dotimes (,sequence-index (array-total-size ,tuple-array))
                     (setf ,tuple-number (aref ,tuple-array ,sequence-index))
                     ,@body))
	     ;; Pop table scope.
	     (pop-table-scope)))
	 ;; Return value of body.
	 (values ,value)))))

