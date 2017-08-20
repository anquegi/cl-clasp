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
;;; Query function for accessing all non-empty tuples in table.
;;;

(defun query-all ()
  "Allocate and return a tuple-set containing all the tuples in the current table."
  (let* ((tbl-name (current-table-name))
	 (table (get-table tbl-name))
	 (attribute (current-attribute (or (first (table-indexed-attr-name-list table))
                                           (first (table-attr-name-list table)))))
	 (index (attribute-index attribute))
	 (tuple-set (tuple-set-allocate)))
    (if index
	;; Use indexed query if possible.
	(index-query-all tuple-set index)
	;; Otherwise, iterate over all tuples looking for non-free tuples.
	(progn
	  (rtm-warning "No index available for query all on table \'" tbl-name ".")
          (do-table-filled-rows (row (table-free-tuple-chain table))
            (tuple-set-union-element tuple-set row))))
    tuple-set))

(defmacro def-queryfn-1 (theta)
  "Generate query function for specified single-attribute, match-all theta clause operator."
  (let* ((pkg (find-package "RTM"))
	 (query-fn (intern (concatenate 'string "QUERY-" theta) pkg))
	 (ind-query-fn (intern (concatenate 'string "INDEX-QUERY-" theta) pkg))
	 (attr-comp-mc (intern (concatenate 'string "ATTR-" theta "-P") pkg)))
    `(progn
       (defun ,query-fn (attr-name attr-value)
	 (let ((tbl-name (current-table-name))
	       (attribute (current-attribute attr-name)))
	   ;; Check for valid attribute in current table.
	   (unless attribute
	     (rtm-error "Invalid query - attribute \'" attr-name
			" does not exist for table \'" tbl-name ".")
	     (return-from ,query-fn (values nil)))
	   (let* ((domain-name (attribute-domain-name attribute))
		  (domain (get-domain domain-name))
		  (equal-p-fn (domain-equal-p domain))
		  (less-p-fn (domain-less-p domain))
		  (int-attr-value (domain-encode-and-verify domain-name attr-value)))
	     ;; Check for comparable attribute domain.
	     (unless ,(if (or (string= theta "EQ") (string= theta "NE"))
			  `equal-p-fn
			  `(attribute-ordered-p attribute))
	       (rtm-error "Invalid query - attribute \'" attr-name
			  " is in noncomparable domain \'" domain-name ".")
	       (return-from ,query-fn (values nil)))
	     ;; Access attribute information to perform search.
	     (let ((index (attribute-index attribute))
		   (tuple-set (tuple-set-allocate)))
	       (if index
		   ;; Use indexed query if possible.
		   (,ind-query-fn tuple-set index int-attr-value equal-p-fn less-p-fn)
		   ;; Otherwise, iterate over all table tuples.  
                   (let* ((data-array (attribute-data attribute))
                          (table (get-table tbl-name))
                          (tuple-count (table-tuple-count table))
                          table-attr-value)
                     (rtm-warning "No index on attribute \'" attr-name
                                  " in query on table \'" tbl-name ".")
                     ;; If another attribute in table has an index, and the total number 
                     ;;   of filled rows is less than half the table size, use query-all 
                     ;;   to generate a source-tuple-set to be traversed.  Otherwise, 
                     ;;   iterate over all table rows directly.
                     (if (and (table-indexed-attr-name-list table)
                              (< (+ tuple-count tuple-count) (table-cardinality table)))
                       ;; Get tuple-set of all tuples first, then collect row numbers 
                       ;;   of tuples with matching attribute values.
                       (let ((total-tuple-set (query-all)))
                         (do-tuple-set (row total-tuple-set)
                           (setf table-attr-value (aref data-array row))
                           (when (,attr-comp-mc table-attr-value int-attr-value
                                                equal-p-fn less-p-fn)
                             (tuple-set-union-element tuple-set row)))
                         (tuple-set-release total-tuple-set))
                       ;; Otherwise, iterate over whole table,
                       ;;   collecting row numbers of filled rows with matching 
                       ;;   table attribute values.
                       (do-table-filled-rows (row (table-free-tuple-chain table))
                         (setf table-attr-value (aref data-array row))
                         (when (,attr-comp-mc table-attr-value int-attr-value
                                              equal-p-fn less-p-fn)
                           (tuple-set-union-element tuple-set row))))))
	       tuple-set)))))))

(defmacro def-queryfn-2 (theta)
  "Generate range query function for specified two-attribute, theta clause operator."
  (let* ((pkg (find-package "RTM"))
	 (query-fn (intern (concatenate 'string "QUERY-" theta) pkg))
	 (ind-query-fn (intern (concatenate 'string "INDEX-QUERY-" theta) pkg))
	 (attr-comp-mc (intern (concatenate 'string "ATTR-" theta "-P") pkg)))
    `(progn
       (defun ,query-fn (attr-name attr-value-1 attr-value-2)
	 (let ((tbl-name (current-table-name))
	       (attribute (current-attribute attr-name)))
	   ;; Check for valid attribute in current table.
	   (unless attribute
	     (rtm-error "Invalid range query - attribute \'" attr-name
			" does not exist for table \'" tbl-name ".")
	     (return-from ,query-fn (values nil)))
	   (let* ((domain-name (attribute-domain-name attribute))
		  (domain (get-domain domain-name))
		  (equal-p-fn (domain-equal-p domain))
		  (less-p-fn (domain-less-p domain))
		  (attr-1 (domain-encode-and-verify domain-name attr-value-1))
		  (attr-2 (domain-encode-and-verify domain-name attr-value-2)))
	     ;; Check for comparable attribute domain.
	     (unless (attribute-ordered-p attribute)
	       (rtm-error "Invalid query - attribute \'" attr-name
			  " is in noncomparable domain \'" domain-name ".")
	       (return-from ,query-fn (values nil)))
	     ;; Encode and validate attribute values.
	     (let (temp)
	       (if (attr-lt-p attr-2 attr-1 equal-p-fn less-p-fn)
		   (setf temp attr-1 attr-1 attr-2 attr-2 temp)))
	     ;; Access attribute information to perform search.
	     (let ((index (attribute-index attribute))
		   (tuple-set (tuple-set-allocate)))
	       (if index
		   ;; Use indexed query if possible.
		   (,ind-query-fn tuple-set index attr-1 attr-2 equal-p-fn less-p-fn)
		   ;; Otherwise, iterate over all table tuples.  
		   (let* ((data-array (attribute-data attribute))
			  (table (get-table tbl-name))
			  (tuple-count (table-tuple-count table))
			  table-attr-value)
		     (rtm-warning "No index on attribute \'" attr-name
				  " in query on table \'" tbl-name ".")
		     ;; If another attribute in table has an index, and the total 
                     ;;    number of filled rows is less than half the table size, 
                     ;;    use query-all to generate a source-tuple-set to be traversed.  
                     ;;    Otherwise, iterate over all table rows directly.
		     (if (and (table-indexed-attr-name-list table)
			      (< (+ tuple-count tuple-count) (table-cardinality table)))
			 ;; Get tuple-set of all tuples first,
			 ;;   then collect row numbers of tuples with matching attribute values.
			 (let ((total-tuple-set (query-all)))
			   (do-tuple-set (row total-tuple-set)
			     (setf table-attr-value (aref data-array row))
			     (when (,attr-comp-mc table-attr-value attr-1 attr-2
				    equal-p-fn less-p-fn)
			       (tuple-set-union-element tuple-set row)))
			   (tuple-set-release total-tuple-set))
			 ;; Otherwise, iterate over whole table,
			 ;;   collecting row numbers of filled rows with matching table attribute values.
			 (do-table-filled-rows (row (table-free-tuple-chain table))
			   (setf table-attr-value (aref data-array row))
			   (when (,attr-comp-mc table-attr-value attr-1 attr-2
				  equal-p-fn less-p-fn)
			     (tuple-set-union-element tuple-set row))))))
	       tuple-set)))))))

;;;
;;; Query functions either match one (best) attr-value or many (all)
;;;   and specify zero, one, or two attributes.
;;;
;;; Single attribute comparison functions:
(def-queryfn-1 "EQ")
(def-queryfn-1 "NE")
(def-queryfn-1 "LT")
(def-queryfn-1 "GE")
(def-queryfn-1 "GT")
(def-queryfn-1 "LE")

;;; Double attribute comparison functions
;;;   Range comparisons - between:
(def-queryfn-2 "GE-LE")
(def-queryfn-2 "GT-LE")
(def-queryfn-2 "GE-LT")
(def-queryfn-2 "GT-LT")
;;;   Range comparisons - not between:
(def-queryfn-2 "LE-GE")
(def-queryfn-2 "LT-GE")
(def-queryfn-2 "LE-GT")
(def-queryfn-2 "LT-GT")

;;; Special attribute comparison functions:

(defun query-min (attr-name)
  (let ((tbl-name (current-table-name))
	(attribute (current-attribute attr-name)))
    ;; Check for valid attribute in current table.
    (unless attribute
      (rtm-error "Invalid query - attribute \'" attr-name
		 " does not exist for table \'" tbl-name ".")
      (return-from query-min (values nil)))
    (let* ((domain-name (attribute-domain-name attribute))
	   (domain (get-domain domain-name))
	   (equal-p-fn (domain-equal-p domain))
	   (less-p-fn (domain-less-p domain)))
      ;; Check for comparable attribute domain.
      (unless (attribute-ordered-p attribute)
	(rtm-error "Invalid query - attribute \'" attr-name
		   " is in noncomparable domain \'" domain-name ".")
	(return-from query-min (values nil)))
      ;; Access attribute information to perform search.
      (let ((index (attribute-index attribute))
	    (tuple-set (tuple-set-allocate)))
	(if index
	    ;; Use indexed query if possible.
	    (index-query-min tuple-set index)
	    ;; Otherwise, iterate over all table tuples.  
	    (let* ((data-array (attribute-data attribute))
		   (table (get-table tbl-name))
		   (tuple-count (table-tuple-count table))
		   table-attr-value min-attr-value)
	      (rtm-warning "No index on attribute \'" attr-name
			   " in query on table \'" tbl-name ".")
	      ;; If another attribute in table has an index, and the total number of filled rows 
	      ;;   in less than half the table size, use query-all to generate a source-tuple-set 
	      ;;   to be traversed.  Otherwise, iterate over all table rows directly.
	      (if (and (table-indexed-attr-name-list table)
		       (< (+ tuple-count tuple-count) (table-cardinality table)))
		  ;; Get tuple-set of all tuples first,
		  ;;   then collect row numbers of tuples with matching attribute values.
		  (let ((total-tuple-set (query-all)))
		    (do-tuple-set (row total-tuple-set)
		      (setf table-attr-value (aref data-array row))
		      (cond ((or (null min-attr-value)
				 (funcall less-p-fn table-attr-value min-attr-value))
			     (setf min-attr-value table-attr-value)
			     (tuple-set-intersect-list tuple-set nil)
			     (tuple-set-union-element tuple-set row))
			    ((funcall equal-p-fn table-attr-value min-attr-value)
			     (tuple-set-union-element tuple-set row))))
		    (tuple-set-release total-tuple-set))
		  ;; Otherwise, iterate over whole table,
		  ;;   collecting row numbers of filled rows with matching table attribute values.
		  (do-table-filled-rows (row (table-free-tuple-chain table))
		    (setf table-attr-value (aref data-array row))
		    (cond ((or (null min-attr-value)
				 (funcall less-p-fn table-attr-value min-attr-value))
			     (setf min-attr-value table-attr-value)
			     (tuple-set-intersect-list tuple-set nil)
			     (tuple-set-union-element tuple-set row))
			    ((funcall equal-p-fn table-attr-value min-attr-value)
			     (tuple-set-union-element tuple-set row)))))))
	tuple-set))))

(defun query-max (attr-name)
  (let ((tbl-name (current-table-name))
	(attribute (current-attribute attr-name)))
    ;; Check for valid attribute in current table.
    (unless attribute
      (rtm-error "Invalid query - attribute \'" attr-name
		 " does not exist for table \'" tbl-name ".")
      (return-from query-max (values nil)))
    (let* ((domain-name (attribute-domain-name attribute))
	   (domain (get-domain domain-name))
	   (equal-p-fn (domain-equal-p domain))
	   (less-p-fn (domain-less-p domain)))
      ;; Check for comparable attribute domain.
      (unless (attribute-ordered-p attribute)
	(rtm-error "Invalid query - attribute \'" attr-name
		   " is in noncomparable domain \'" domain-name ".")
	(return-from query-max (values nil)))
      ;; Access attribute information to perform search.
      (let ((index (attribute-index attribute))
	    (tuple-set (tuple-set-allocate)))
	(if index
	    ;; Use indexed query if possible.
	    (index-query-max tuple-set index)
	    ;; Otherwise, iterate over all table tuples.  
	    (let* ((data-array (attribute-data attribute))
		   (table (get-table tbl-name))
		   (tuple-count (table-tuple-count table))
		   table-attr-value max-attr-value)
	      (rtm-warning "No index on attribute \'" attr-name
			   " in query on table \'" tbl-name ".")
	      ;; If another attribute in table has an index, and the total number of filled rows 
	      ;;   in less than half the table size, use query-all to generate a source-tuple-set 
	      ;;   to be traversed.  Otherwise, iterate over all table rows directly.
	      (if (and (table-indexed-attr-name-list table)
		       (< (+ tuple-count tuple-count) (table-cardinality table)))
		  ;; Get tuple-set of all tuples first,
		  ;;   then collect row numbers of tuples with matching attribute values.
		  (let ((total-tuple-set (query-all)))
		    (do-tuple-set (row total-tuple-set)
		      (setf table-attr-value (aref data-array row))
		      (cond ((or (null max-attr-value)
				 (funcall less-p-fn max-attr-value table-attr-value))
			     (setf max-attr-value table-attr-value)
			     (tuple-set-intersect-list tuple-set nil)
			     (tuple-set-union-element tuple-set row))
			    ((funcall equal-p-fn table-attr-value max-attr-value)
			     (tuple-set-union-element tuple-set row))))
		    (tuple-set-release total-tuple-set))
		  ;; Otherwise, iterate over whole table,
		  ;;   collecting row numbers of filled rows with matching table attribute values.
		  (do-table-filled-rows (row (table-free-tuple-chain table))
		    (setf table-attr-value (aref data-array row))
		    (cond ((or (null max-attr-value)
				 (funcall less-p-fn max-attr-value table-attr-value))
			     (setf max-attr-value table-attr-value)
			     (tuple-set-intersect-list tuple-set nil)
			     (tuple-set-union-element tuple-set row))
			    ((funcall equal-p-fn table-attr-value max-attr-value)
			     (tuple-set-union-element tuple-set row)))))))
	tuple-set))))

(defun query-pred (attr-name attr-value)
  (let ((tbl-name (current-table-name))
	(attribute (current-attribute attr-name)))
    ;; Check for valid attribute in current table.
    (unless attribute
      (rtm-error "Invalid query - attribute \'" attr-name
		 " does not exist for table \'" tbl-name ".")
      (return-from query-pred (values nil)))
    (let* ((domain-name (attribute-domain-name attribute))
	   (domain (get-domain domain-name))
	   (equal-p-fn (domain-equal-p domain))
	   (less-p-fn (domain-less-p domain))
	   (int-attr-value (domain-encode-and-verify domain-name attr-value)))
      ;; Check for comparable attribute domain.
      (unless (attribute-ordered-p attribute)
	(rtm-error "Invalid query - attribute \'" attr-name
		   " is in noncomparable domain \'" domain-name ".")
	(return-from query-pred (values nil)))
      ;; Access attribute information to perform search.
      (let ((index (attribute-index attribute))
	    (tuple-set (tuple-set-allocate)))
	(if index
	    ;; Use indexed query if possible.
	    (index-query-pred tuple-set index int-attr-value equal-p-fn less-p-fn)
	    ;; Otherwise, iterate over all table tuples.  
	    (let* ((data-array (attribute-data attribute))
		   (table (get-table tbl-name))
		   (tuple-count (table-tuple-count table))
		   table-attr-value pred-attr-value)
	      (rtm-warning "No index on attribute \'" attr-name
			   " in query on table \'" tbl-name ".")
	      ;; Get min as initial predecessor.
	      (let ((temp-tuple-set (query-min attr-name))
		    temp-attr-value)
		(do-tuple-set (row temp-tuple-set)
		  (setf temp-attr-value (aref data-array row))
		  (when (attr-lt-p temp-attr-value int-attr-value equal-p-fn less-p-fn)
		    (setf pred-attr-value temp-attr-value))
		  (return))
		(tuple-set-release temp-tuple-set))
	      ;; If another attribute in table has an index, and the total number of filled rows 
	      ;;   in less than half the table size, use query-all to generate a source-tuple-set 
	      ;;   to be traversed.  Otherwise, iterate over all table rows directly.
	      (if (and (table-indexed-attr-name-list table)
		       (< (+ tuple-count tuple-count) (table-cardinality table)))
		  ;; Get tuple-set of all tuples first,
		  ;;   then collect row numbers of tuples with matching attribute values.
		  (let ((total-tuple-set (query-all)))
		    (do-tuple-set (row total-tuple-set)
		      (setf table-attr-value (aref data-array row))
		      (cond ((attr-gt-lt-p table-attr-value pred-attr-value
					   int-attr-value equal-p-fn less-p-fn)
			     (setf pred-attr-value table-attr-value)
			     (tuple-set-intersect-list tuple-set nil)
			     (tuple-set-union-element tuple-set row))
			    ((and pred-attr-value
				  (funcall equal-p-fn table-attr-value pred-attr-value))
			     (tuple-set-union-element tuple-set row))))
		    (tuple-set-release total-tuple-set))
		  ;; Otherwise, iterate over whole table,
		  ;;   collecting row numbers of filled rows with matching table attribute values.
		  (do-table-filled-rows (row (table-free-tuple-chain table))
		    (setf table-attr-value (aref data-array row))
		    (cond ((attr-gt-lt-p table-attr-value pred-attr-value
					 int-attr-value equal-p-fn less-p-fn)
			   (setf pred-attr-value table-attr-value)
			   (tuple-set-intersect-list tuple-set nil)
			   (tuple-set-union-element tuple-set row))
			  ((and pred-attr-value
				(funcall equal-p-fn table-attr-value pred-attr-value))
			   (tuple-set-union-element tuple-set row)))))))
	tuple-set))))

(defun query-succ (attr-name attr-value)
  (let ((tbl-name (current-table-name))
	(attribute (current-attribute attr-name)))
    ;; Check for valid attribute in current table.
    (unless attribute
      (rtm-error "Invalid query - attribute \'" attr-name
		 " does not exist for table \'" tbl-name ".")
      (return-from query-succ (values nil)))
    (let* ((domain-name (attribute-domain-name attribute))
	   (domain (get-domain domain-name))
	   (equal-p-fn (domain-equal-p domain))
	   (less-p-fn (domain-less-p domain))
	   (int-attr-value (domain-encode-and-verify domain-name attr-value)))
      ;; Check for comparable attribute domain.
      (unless (attribute-ordered-p attribute)
	(rtm-error "Invalid query - attribute \'" attr-name
		   " is in noncomparable domain \'" domain-name ".")
	(return-from query-succ (values nil)))
      ;; Access attribute information to perform search.
      (let ((index (attribute-index attribute))
	    (tuple-set (tuple-set-allocate)))
	(if index
	    ;; Use indexed query if possible.
	    (index-query-succ tuple-set index int-attr-value equal-p-fn less-p-fn)
	    ;; Otherwise, iterate over all table tuples.  
	    (let* ((data-array (attribute-data attribute))
		   (table (get-table tbl-name))
		   (tuple-count (table-tuple-count table))
		   table-attr-value succ-attr-value)
	      (rtm-warning "No index on attribute \'" attr-name
			   " in query on table \'" tbl-name ".")
	      ;; Get max as initial successor.
	      (let ((temp-tuple-set (query-max attr-name))
		    temp-attr-value)
		(do-tuple-set (row temp-tuple-set)
		  (setf temp-attr-value (aref data-array row))
		  (when (attr-gt-p temp-attr-value int-attr-value equal-p-fn less-p-fn)
		    (setf succ-attr-value temp-attr-value))
		  (return))
		(tuple-set-release temp-tuple-set))
	      ;; If another attribute in table has an index, and the total number of filled rows 
	      ;;   in less than half the table size, use query-all to generate a source-tuple-set 
	      ;;   to be traversed.  Otherwise, iterate over all table rows directly.
	      (if (and (table-indexed-attr-name-list table)
		       (< (+ tuple-count tuple-count) (table-cardinality table)))
		  ;; Get tuple-set of all tuples first,
		  ;;   then collect row numbers of tuples with matching attribute values.
		  (let ((total-tuple-set (query-all)))
		    (do-tuple-set (row total-tuple-set)
		      (setf table-attr-value (aref data-array row))
		      (cond ((attr-gt-lt-p table-attr-value int-attr-value
					   succ-attr-value equal-p-fn less-p-fn)
			     (setf succ-attr-value table-attr-value)
			     (tuple-set-intersect-list tuple-set nil)
			     (tuple-set-union-element tuple-set row))
			    ((and succ-attr-value
				  (funcall equal-p-fn table-attr-value succ-attr-value))
			     (tuple-set-union-element tuple-set row))))
		    (tuple-set-release total-tuple-set))
		  ;; Otherwise, iterate over whole table,
		  ;;   collecting row numbers of filled rows with matching table attribute values.
		  (do-table-filled-rows (row (table-free-tuple-chain table))
		    (setf table-attr-value (aref data-array row))
		    (cond ((attr-gt-lt-p table-attr-value int-attr-value
					 succ-attr-value equal-p-fn less-p-fn)
			   (setf succ-attr-value table-attr-value)
			   (tuple-set-intersect-list tuple-set nil)
			   (tuple-set-union-element tuple-set row))
			  ((and succ-attr-value
				(funcall equal-p-fn table-attr-value succ-attr-value))
			   (tuple-set-union-element tuple-set row)))))))
	tuple-set))))

(defun query-floor (attr-name attr-value)
  (let ((tbl-name (current-table-name))
	(attribute (current-attribute attr-name)))
    ;; Check for valid attribute in current table.
    (unless attribute
      (rtm-error "Invalid query - attribute \'" attr-name
		 " does not exist for table \'" tbl-name ".")
      (return-from query-floor (values nil)))
    (let* ((domain-name (attribute-domain-name attribute))
	   (domain (get-domain domain-name))
	   (equal-p-fn (domain-equal-p domain))
	   (less-p-fn (domain-less-p domain))
	   (int-attr-value (domain-encode-and-verify domain-name attr-value)))
      ;; Check for comparable attribute domain.
      (unless (attribute-ordered-p attribute)
	(rtm-error "Invalid query - attribute \'" attr-name
		   " is in noncomparable domain \'" domain-name ".")
	(return-from query-floor (values nil)))
      ;; Access attribute information to perform search.
      (let ((index (attribute-index attribute))
	    (tuple-set (tuple-set-allocate)))
	(if index
	    ;; Use indexed query if possible.
	    (index-query-floor tuple-set index int-attr-value equal-p-fn less-p-fn)
	    ;; Otherwise, iterate over all table tuples.  
	    (let* ((data-array (attribute-data attribute))
		   (table (get-table tbl-name))
		   (tuple-count (table-tuple-count table))
		   table-attr-value floor-attr-value)
	      (rtm-warning "No index on attribute \'" attr-name
			   " in query on table \'" tbl-name ".")
	      ;; Get min as initial floor.
	      (let ((temp-tuple-set (query-min attr-name))
		    temp-attr-value)
		(do-tuple-set (row temp-tuple-set)
		  (setf temp-attr-value (aref data-array row))
		  (when (attr-le-p temp-attr-value int-attr-value equal-p-fn less-p-fn)
		    (setf floor-attr-value temp-attr-value))
		  (return))
		(tuple-set-release temp-tuple-set))
	      ;; If another attribute in table has an index, and the total number of filled rows 
	      ;;   in less than half the table size, use query-all to generate a source-tuple-set 
	      ;;   to be traversed.  Otherwise, iterate over all table rows directly.
	      (if (and (table-indexed-attr-name-list table)
		       (< (+ tuple-count tuple-count) (table-cardinality table)))
		  ;; Get tuple-set of all tuples first,
		  ;;   then collect row numbers of tuples with matching attribute values.
		  (let ((total-tuple-set (query-all)))
		    (do-tuple-set (row total-tuple-set)
		      (setf table-attr-value (aref data-array row))
		      (cond ((attr-gt-le-p table-attr-value floor-attr-value
					   int-attr-value equal-p-fn less-p-fn)
			     (setf floor-attr-value table-attr-value)
			     (tuple-set-intersect-list tuple-set nil)
			     (tuple-set-union-element tuple-set row))
			    ((and floor-attr-value
				  (funcall equal-p-fn table-attr-value floor-attr-value))
			     (tuple-set-union-element tuple-set row))))
		    (tuple-set-release total-tuple-set))
		  ;; Otherwise, iterate over whole table,
		  ;;   collecting row numbers of filled rows with matching table attribute values.
		  (do-table-filled-rows (row (table-free-tuple-chain table))
		    (setf table-attr-value (aref data-array row))
		    (cond ((attr-gt-le-p table-attr-value floor-attr-value
					 int-attr-value equal-p-fn less-p-fn)
			   (setf floor-attr-value table-attr-value)
			   (tuple-set-intersect-list tuple-set nil)
			   (tuple-set-union-element tuple-set row))
			  ((and floor-attr-value
				(funcall equal-p-fn table-attr-value floor-attr-value))
			   (tuple-set-union-element tuple-set row)))))))
	tuple-set))))

(defun query-ceiling (attr-name attr-value)
  (let ((tbl-name (current-table-name))
	(attribute (current-attribute attr-name)))
    ;; Check for valid attribute in current table.
    (unless attribute
      (rtm-error "Invalid query - attribute \'" attr-name
		 " does not exist for table \'" tbl-name ".")
      (return-from query-ceiling (values nil)))
    (let* ((domain-name (attribute-domain-name attribute))
	   (domain (get-domain domain-name))
	   (equal-p-fn (domain-equal-p domain))
	   (less-p-fn (domain-less-p domain))
	   (int-attr-value (domain-encode-and-verify domain-name attr-value)))
      ;; Check for comparable attribute domain.
      (unless (attribute-ordered-p attribute)
	(rtm-error "Invalid query - attribute \'" attr-name
		   " is in noncomparable domain \'" domain-name ".")
	(return-from query-ceiling (values nil)))
      ;; Access attribute information to perform search.
      (let ((index (attribute-index attribute))
	    (tuple-set (tuple-set-allocate)))
	(if index
	    ;; Use indexed query if possible.
	    (index-query-ceiling tuple-set index int-attr-value equal-p-fn less-p-fn)
	    ;; Otherwise, iterate over all table tuples.  
	    (let* ((data-array (attribute-data attribute))
		   (table (get-table tbl-name))
		   (tuple-count (table-tuple-count table))
		   table-attr-value ceiling-attr-value)
	      (rtm-warning "No index on attribute \'" attr-name
			   " in query on table \'" tbl-name ".")
	      ;; Get max as initial ceiling.
	      (let ((temp-tuple-set (query-max attr-name))
		    temp-attr-value)
		(do-tuple-set (row temp-tuple-set)
		  (setf temp-attr-value (aref data-array row))
		  (when (attr-ge-p temp-attr-value int-attr-value equal-p-fn less-p-fn)
		    (setf ceiling-attr-value temp-attr-value))
		  (return))
		(tuple-set-release temp-tuple-set))
	      ;; If another attribute in table has an index, and the total number of filled rows 
	      ;;   in less than half the table size, use query-all to generate a source-tuple-set 
	      ;;   to be traversed.  Otherwise, iterate over all table rows directly.
	      (if (and (table-indexed-attr-name-list table)
		       (< (+ tuple-count tuple-count) (table-cardinality table)))
		  ;; Get tuple-set of all tuples first,
		  ;;   then collect row numbers of tuples with matching attribute values.
		  (let ((total-tuple-set (query-all)))
		    (do-tuple-set (row total-tuple-set)
		      (setf table-attr-value (aref data-array row))
		      (cond ((attr-ge-lt-p table-attr-value int-attr-value
					   ceiling-attr-value equal-p-fn less-p-fn)
			     (setf ceiling-attr-value table-attr-value)
			     (tuple-set-intersect-list tuple-set nil)
			     (tuple-set-union-element tuple-set row))
			    ((and ceiling-attr-value
				  (funcall equal-p-fn table-attr-value ceiling-attr-value))
			     (tuple-set-union-element tuple-set row))))
		    (tuple-set-release total-tuple-set))
		  ;; Otherwise, iterate over whole table,
		  ;;   collecting row numbers of filled rows with matching table attribute values.
		  (do-table-filled-rows (row (table-free-tuple-chain table))
		    (setf table-attr-value (aref data-array row))
		    (cond ((attr-ge-lt-p table-attr-value int-attr-value
					 ceiling-attr-value equal-p-fn less-p-fn)
			   (setf ceiling-attr-value table-attr-value)
			   (tuple-set-intersect-list tuple-set nil)
			   (tuple-set-union-element tuple-set row))
			  ((and ceiling-attr-value
				(funcall equal-p-fn table-attr-value ceiling-attr-value))
			   (tuple-set-union-element tuple-set row)))))))
	tuple-set))))

;;;
;;; Distinct selection function:
;;;

(defun distinct-tuple-set (tuple-set attr-list)
  "Remove any duplicates on attr-list values from tuple-set."
  (when (symbolp attr-list)
    (setf attr-list (list attr-list)))
  (let* ((num-attrs (length attr-list))
         (distinct-table (make-hash-table
                          :test #'equal 
                          :size (tuple-set-cardinality tuple-set)))
         (col-array (make-array num-attrs))
         (value-list nil)
         (new-tuple-set nil))
    ;; Set up data column and value arrays with 1 element per distinct attribute.
    (do* ((i 0 (+ i 1))
          (attrs attr-list (rest attrs))
          (attr-name (first attrs) (first attrs)))
         ((null attrs))
      (let ((attribute (current-attribute attr-name)))
        ;; Validate attr-name for current table.
        (unless attribute
          (rtm-warning "Attribute value access returning nil - invalid attribute \'" attr-name
                       " for table \'" (current-table-name) ".")
          (return-from distinct-tuple-set (values tuple-set)))
        (setf (aref col-array i) (attribute-data attribute))))
    ;; Iterate over each tuple in tuple-set, putting attr values into
    ;;   value array and hashing.  If no collision occurs, retain tuple-id
    ;;   as distinct and mark hash location with tuple-id.
    (setf new-tuple-set (tuple-set-allocate))
    (do-tuple-set (tuple-number tuple-set)
      (setf value-list nil)
      (dotimes (i num-attrs)
        (push (aref (aref col-array i) tuple-number) value-list))
      (unless (gethash value-list distinct-table)
        (tuple-set-union-element new-tuple-set tuple-number)
        (setf (gethash value-list distinct-table) tuple-number)))
    (tuple-set-release tuple-set)
    (values new-tuple-set)))

;;;
;;; Sorting function:
;;;

(defun sort-tuples (tuples attr-ordering)
  "Sort tuples according to specified attribute ordering."
  ;; tuples is either an array of tuple numbers or a tuple-set.
  (let ((tuple-sequence (if (tuple-set-p tuples) (tuple-set-array tuples) tuples)))
    ;; If single attribute, convert to list.
    (unless (listp attr-ordering)
      (setf attr-ordering (list attr-ordering)))
    ;; Check for unordered attributes.
    (dolist (attr attr-ordering)
      (unless (or (eq attr :asc)
		  (eq attr :desc))
	(let ((attribute (current-attribute attr)))
	  (unless attribute
	    (rtm-warning "Can't sort on attribute \'" attr " in table \'"
			 (current-table-name) " - attribute not in table.")
	    (return-from sort-tuples tuple-sequence))
	  (unless (attribute-ordered-p attribute)
	    (rtm-warning "Can't sort on attribute \'" attr " in table \'"
			 (current-table-name) " - unordered domain \'"
			 (attribute-domain-name attribute) ".")
	    (return-from sort-tuples tuple-sequence))))) 
    ;; Sort tuple-numbers according to specified attr-ordering.
    (sort tuple-sequence
	  #'(lambda (tuple-number-1 tuple-number-2)
	      (let (descending-p attr-1 attr-2)
		(dolist (attr attr-ordering)
		  (cond ((eq attr :desc)
			 (setf descending-p t))
			((eq attr :asc)
			 (setf descending-p nil))
			(t
			 (let* ((attribute (current-attribute attr))
				(data-array (attribute-data attribute))
				(domain-name (attribute-domain-name attribute))
				(domain (get-domain domain-name))
				(equal-p (domain-equal-p domain))
				(less-p (domain-less-p domain)))
			   (if descending-p
			       (setf attr-1 (aref data-array tuple-number-2)
				     attr-2 (aref data-array tuple-number-1))
			       (setf attr-1 (aref data-array tuple-number-1)
				     attr-2 (aref data-array tuple-number-2)))
			   (cond ((funcall less-p attr-1 attr-2)
				  (return t))
				 ((attr-gt-p attr-1 attr-2 equal-p less-p)
				  (return nil))))))))))
    tuple-sequence))

;;;
;;; Nearest neighbor query mechanism:
;;;

(defstruct nn
  priority-queue
  attr-count
  attr-array
  candidates
  target-count
  distance-fn
  pt-args
  boundary-pt-args)

(defstruct nn-attr
  (data nil)
  (index nil)
  (lower-pos nil)
  (upper-pos nil))

(defmacro make-neighbor (&key (distance 0.0) (tuple-number 0))
  `(cons ,distance ,tuple-number))

(defmacro neighbor-distance (neighbor)
  `(car ,neighbor))

(defmacro neighbor-tuple-number (neighbor)
  `(cdr ,neighbor))

(defstruct priority-queue
  (heap (make-array *rtm-initial-nn-priority-queue-size*))
  (length 0))

(defun priority-queue-allocate ()
  "Return an empty priority-queue."
  (let (priority-queue)
    ;; If no more priority-queues on list, one is created.
    (setf priority-queue (if (= 0 (fill-pointer *rtm-priority-queues*)) 
			     (make-priority-queue)
			     (vector-pop *rtm-priority-queues*)))
    ;; Make priority-queue empty by setting length to 0.
    (setf (priority-queue-length priority-queue) 0)
    priority-queue))

(defun priority-queue-release (priority-queue)
  "Return priority-queue to pool of available priority-queues."
  ;; Push released priority-queue onto list of free priority-queues.
  (vector-push-extend priority-queue *rtm-priority-queues*)
  nil)

(defun priority-queue-insert (distance tuple-number priority-queue)
  "Insert an element into a priority queue."
  (let* ((length (incf (priority-queue-length priority-queue)))
	 (heap (priority-queue-heap priority-queue))
	 (current-size (array-total-size heap))
	 element)
    (when (<= current-size length)
      (adjust-array heap (round (* current-size *rtm-table-expansion-factor*))))
    (if (setf element (aref heap length))
	(setf (neighbor-distance element) distance
	      (neighbor-tuple-number element) tuple-number)
	(setf (aref heap length)
	      (make-neighbor :distance distance :tuple-number tuple-number)))
    (nn-siftup heap length)))

(defun priority-queue-pop (priority-queue)
  "Pop an element from a priority-queue"
  (let* ((old-length (priority-queue-length priority-queue))
	 (new-length (1- old-length))
	 (heap (priority-queue-heap priority-queue))
	 (top (aref heap 1)))
    (setf (priority-queue-length priority-queue) new-length)
    (when (< new-length 0)
      (setf (priority-queue-length priority-queue) 0)
      (error "Attempt to pop empty priority queue"))
    (setf (aref heap 1) (aref heap old-length)
	  (aref heap old-length) top)
    (nn-siftdown heap new-length)
    (values top)))

(defun priority-queue-head (priority-queue)
  "Return the first element in the priority queue"
  (when (< 0 (priority-queue-length priority-queue))
    (aref (priority-queue-heap priority-queue) 1)))

(defun nn-siftup (heap n)
  "Move heap(n) up to correct place"
  (let ((i n)
	(elt (aref heap n)))
    (do (p)
	((= i 1))
      (setf p (truncate (/ i 2)))
      (unless (< (neighbor-distance elt)
		 (neighbor-distance (aref heap p)))
	(return))
      (setf (aref heap i) (aref heap p))
      (setf i p))
    (setf (aref heap i) elt)))

(defun nn-siftdown (heap n)
  "Move heap(1) downto the correct place"
  (let ((i 1)
	(newelement (aref heap 1)))
    (do (heapc
	 c)
	(nil)
      (setf c (+ i i))
      (cond ((<= n c)
	     ;; either last element or out of bounds
	     (when (< n c)
	       (return))
	     ;; last element
	     (setf heapc (aref heap c)))
	    ;; c+1 is guarenteed to be in bounds
	    ((< (neighbor-distance (aref heap (1+ c)))
		(neighbor-distance (setf heapc (aref heap c))))
	     (setf heapc (aref heap (incf c)))))
      (unless (< (neighbor-distance heapc)
		 (neighbor-distance newelement))
	(return))
      (setf (aref heap i) heapc
	    i c))
    (setf (aref heap i) newelement)))

(defun nn-init (from-point distance-metric)
  "Initialize nearest-neighbor structures.  If attributes in
from-point are not indexed, generate an error and return nil."
  (let* ((nn-rec (make-nn))
	 (priority-queue (priority-queue-allocate))
	 (candidates (tuple-set-allocate))
         (attr-double-count (length from-point))
	 (attr-count (/ attr-double-count 2))
	 (attr-array (make-array attr-count))
	 (distance-fn nil)
	 (attr-number 0)
	 (pt-args (make-array attr-double-count))
	 (boundary-pt-args (make-array attr-double-count)))
    ;; Initialize nearest neighbor record.
    (setf (nn-priority-queue nn-rec) priority-queue)
    (setf (nn-candidates nn-rec) candidates)
    (setf (nn-target-count nn-rec) (+ (tuple-set-counter candidates) attr-count))
    (setf (nn-attr-count nn-rec) attr-count)
    (setf (nn-attr-array nn-rec) attr-array)
    (when (null distance-metric)
      (setf distance-metric :euclidean))
    (case distance-metric
      (:city-block (setf distance-fn #'city-block-metric))
      (:euclidean (setf distance-fn #'euclidean-metric))
      (:euclidean-squared (setf distance-fn #'euclidean-squared-metric))
      (t (rtm-warning "Invalid distance-metric \'" distance-metric
		   " specified in do-neighbors - using \:euclidean.")
	 (setf distance-fn #'euclidean-metric)))
    (setf (nn-distance-fn nn-rec) distance-fn)
    ;; Initialize attr-array with indices and boundary positions.
    (do* ((attrs from-point (rest (rest attrs)))
	  (attr-name (first attrs) (first attrs))
	  (ext-attr-value (second attrs) (second attrs)))
	 ((null attrs))
      (let ((attribute (current-attribute attr-name)))
	;; Check for valid attribute name.
	(unless attribute
	  (rtm-warning "Nearest neighbor search failed - invalid attribute \'" attr-name
		       " for table \'" (current-table-name) ".")
	  (return-from nn-init (values nil)))
	(let* ((index (attribute-index attribute))
	       (domain-name (attribute-domain-name attribute))
	       (domain (get-domain domain-name))
	       (equal-p-fn (domain-equal-p domain))
	       (less-p-fn (domain-less-p domain))
	       (int-attr-value (domain-encode-and-verify domain-name ext-attr-value)))
	  ;; Check for indexed attribute.
	  (unless index 
	    (rtm-warning "Nearest neighbor search failed - attribute \'" attr-name
			 " in table \'" (current-table-name) " is not indexed.")
	    (return-from nn-init (values nil)))
	  (let ((node-pos (index-find index int-attr-value equal-p-fn less-p-fn))
		(upper-pos nil)
		(lower-pos nil))
	    ;; Put query point attr value in point arg lists for distance calculations.
	    (setf (aref pt-args (+ attr-count attr-number)) int-attr-value
		  (aref boundary-pt-args (+ attr-count attr-number)) int-attr-value
		  (aref boundary-pt-args attr-number) int-attr-value)
	    (if node-pos
		;; Find leaf node(s) for index upper and lower interval bounds.
		(let ((node (index-node index node-pos)))
		  (if (index-node-leaf-p node)
		      (setf upper-pos node-pos
			    lower-pos node-pos)
		      (setf upper-pos (index-node-descendant-pos
					node int-attr-value equal-p-fn less-p-fn)
			    lower-pos (if upper-pos
					  (index-node-pred-leaf-pos
					    (index-node index upper-pos))
					  (index-max-attr-leaf-pos index)))))
		;; Handle case where index has only one leaf node.
		(if (funcall less-p-fn int-attr-value
			     (index-node-attr-value
			       (index-node index (index-max-attr-leaf-pos index)))) 
		    (setf upper-pos (index-max-attr-leaf-pos index))
		    (setf lower-pos (index-min-attr-leaf-pos index))))
	    ;; Create and initialize attr-array node.
	    (setf (aref attr-array attr-number) (make-nn-attr
						  :data (attribute-data attribute)
						  :index index
						  :lower-pos lower-pos 
						  :upper-pos upper-pos)))))
      ;; Bump attr-number counter for next attribute in point alist.
      (incf attr-number))
    ;; Set query point argument lists for calls to distance function.
    (setf (nn-pt-args nn-rec) pt-args
	  (nn-boundary-pt-args nn-rec) boundary-pt-args)
    ;; Insert initial set of candidates.
    (dotimes (attr-number attr-count)
      (let* ((nn-attr-rec (aref attr-array attr-number))
	     (index (nn-attr-index nn-attr-rec))
	     (lower-pos (nn-attr-lower-pos nn-attr-rec))
	     (upper-pos (nn-attr-upper-pos nn-attr-rec)))
	;; Nominate candidates obtained from lower boundary point.
	(when lower-pos
	  (dolist (tuple-number (index-node-tuple-list (index-node index lower-pos)))
	    (nn-nominate-candidate nn-rec tuple-number)))
	;; Nominate candidates obtained from upper boundary point.
	(when (and upper-pos (or (and lower-pos (/= lower-pos upper-pos))
				 (null lower-pos)))
	  (dolist (tuple-number (index-node-tuple-list (index-node index upper-pos)))
	    (nn-nominate-candidate nn-rec tuple-number)))))
    nn-rec))

(defun nn-extend-interval-up (nn-rec attr-number)
  ;; Extend selected interval upwards, nominating point candidates in extension.
  (let* ((nn-attr-rec (aref (nn-attr-array nn-rec) attr-number))
	 (index (nn-attr-index nn-attr-rec))
	 (success nil)
	 bound-pos node-pos)
    ;; To extend upward, use index-node successor.
    (when (and (setf bound-pos (nn-attr-upper-pos nn-attr-rec))
	       (setf node-pos (index-node-succ-leaf-pos (index-node index bound-pos))
		     (nn-attr-upper-pos nn-attr-rec) node-pos))
      (setf success t)
      (dolist (tuple-number (index-node-tuple-list (index-node index node-pos)))
	(nn-nominate-candidate nn-rec tuple-number)))
    success))

(defun nn-extend-interval-down (nn-rec attr-number)
  ;; Extend selected interval downwards, nominating point candidates in extension.
  (let* ((nn-attr-rec (aref (nn-attr-array nn-rec) attr-number))
	 (index (nn-attr-index nn-attr-rec))
	 (success nil)
	 bound-pos node-pos)
    ;; To extend downward, use index-node predecessor.
    (when (and (setf bound-pos (nn-attr-lower-pos nn-attr-rec))
	       (setf node-pos (index-node-pred-leaf-pos (index-node index bound-pos))
		     (nn-attr-lower-pos nn-attr-rec) node-pos))
      (setf success t)
      (dolist (tuple-number (index-node-tuple-list (index-node index node-pos)))
	(nn-nominate-candidate nn-rec tuple-number)))
    success))

(defun nn-extend-intervals (nn-rec)
  ;; For each attribute, extend intervals up and down, 
  ;;    nominating point candidates in extended intervals.
  (let ((attr-count (nn-attr-count nn-rec))
	(success nil))
    (dotimes (attr-number attr-count)
      (when (nn-extend-interval-up nn-rec attr-number)
	(setf success t))
      (when (nn-extend-interval-down nn-rec attr-number)
	(setf success t)))
    success))

(defun nn-nominate-candidate (nn-rec tuple-number)
  ;; Insert a "nomination" for tuple-number into candidate tuple-set by incrementing
  ;; count.  If all attributes have nominated this candidate (count = target-count),
  ;; calculate distance and promote candidate to priority queue.
  (let* ((tuple-set (nn-candidates nn-rec))
	 (init-count (tuple-set-counter tuple-set)))
    (unless (> (aref (tuple-set-value-array tuple-set) tuple-number) init-count)
      (setf (aref (tuple-set-value-array tuple-set) tuple-number) init-count))
    (incf (aref (tuple-set-value-array tuple-set) tuple-number))
    (when (= (nn-target-count nn-rec)
	     (aref (tuple-set-value-array tuple-set) tuple-number))
      (let ((attr-count (nn-attr-count nn-rec))
	    (attr-array (nn-attr-array nn-rec))
	    (pt-args (nn-pt-args nn-rec)))
	;; Reset tuple-set count to ensure tuple-set integrity.
	(setf (aref (tuple-set-value-array tuple-set) tuple-number) init-count)
	;; Calculate distance from query point and add tuple-number to priority queue.
	(dotimes (i attr-count)
	  (setf (aref pt-args i)
		(aref (nn-attr-data (aref attr-array i)) tuple-number)))
	(priority-queue-insert
	  (funcall (nn-distance-fn nn-rec) pt-args attr-count)
	  tuple-number
	  (nn-priority-queue nn-rec))))))

(defun nn-next (nn-rec)
  ;; Return next nearest neighbor, or nil if no more.
  (let ((priority-queue (nn-priority-queue nn-rec))
	(boundary-pt-args (nn-boundary-pt-args nn-rec))
	neighbor)
    (loop
      ;; If a candidate exists, make sure it's nearest by checking interval boundary distances.
      (when (priority-queue-head priority-queue)
	;; Check each attribute of candidate, extending any interval
	;; boundary that is less than candidate distance away.
	(let ((distance (neighbor-distance (priority-queue-head priority-queue)))
	      (attr-count (nn-attr-count nn-rec)))
	  (dotimes (attr-number attr-count)
	    (let* ((nn-attr-rec (aref (nn-attr-array nn-rec) attr-number))
		   (index (nn-attr-index nn-attr-rec))
		   node-pos)
	      ;; Put attribute value for upper index position in arg-list for distance function, and
	      ;; extend interval until distance is greater than candidate distance or at end of range.
	      (loop
		(if (and (setf node-pos (nn-attr-upper-pos nn-attr-rec))
			 (setf (aref boundary-pt-args attr-number)
			       (index-node-attr-value (index-node index node-pos)))
			 (< (funcall (nn-distance-fn nn-rec) boundary-pt-args attr-count)
			    distance))
		    (nn-extend-interval-up nn-rec attr-number)
		    (return)))
	      ;; Put attribute value for lower index position in arg-list for distance function, and	      
	      ;; extend interval until distance is greater than candidate distance or at end of range.
	      (loop
		(if (and (setf node-pos (nn-attr-lower-pos nn-attr-rec))
			 (setf (aref boundary-pt-args attr-number)
			       (index-node-attr-value (index-node index node-pos)))
			 (< (funcall (nn-distance-fn nn-rec) boundary-pt-args attr-count)
			    distance))
		    (nn-extend-interval-down nn-rec attr-number)
		    (return))))
	    ;; Restore query point value for this attribute.
	    (setf (aref boundary-pt-args attr-number)
		  (aref boundary-pt-args (+ attr-count attr-number)))))
	;; Get newest head of priority queue as neighbor.
	(setf neighbor (priority-queue-head priority-queue))
	(priority-queue-pop priority-queue)
	(return))
      ;; If no candidate neighbors in queue, extend all attribute intervals to nominate
      ;; new candidates.  Exit loop if all intervals have been completely extended.
      (unless (nn-extend-intervals nn-rec)
	(setf neighbor nil)
	(return)))
    neighbor))

(defun city-block-metric (arg-array num-attrs)
  "Compute cit-block distance in n-dimensional minkowski metric space."
  (let ((sum 0))
    (do ((i 0 (+ i 1))
	 (j num-attrs (+ j 1)))
	((= i num-attrs))
      (let* ((xi (aref arg-array i))
	     (xj (aref arg-array j))
	     (delta (abs (- xi xj))))
	(incf sum delta)))
    sum))

(defun euclidean-squared-metric (arg-array num-attrs)
  "Compute euclidean distance squared in n-dimensional minkowski metric space."
  (let ((sum-of-squares 0))
    (do ((i 0 (+ i 1))
	 (j num-attrs (+ j 1)))
	((= i num-attrs))
      (let* ((xi (aref arg-array i))
	     (xj (aref arg-array j))
	     (delta (abs (- xi xj))))
	(when (/= 0 delta)
	  (incf sum-of-squares (* delta delta)))))
    sum-of-squares))

(defun euclidean-metric (arg-array num-attrs)
  "Compute euclidean distance in n-dimensional minkowski metric space."
  (let ((sum-of-squares 0))
    (do ((i 0 (+ i 1))
	 (j num-attrs (+ j 1)))
	((= i num-attrs))
      (let* ((xi (aref arg-array i))
	     (xj (aref arg-array j))
	     (delta (abs (- xi xj))))
	(when (/= 0 delta)
	  (incf sum-of-squares (* delta delta)))))
    (sqrt sum-of-squares)))

(defun nn-minkowski-distance (arg-array num-attrs metric-order)
  "Compute distance in n-dimensional minkowski metric space."
  (let ((sum-of-powers 0))
    (do ((i 0 (+ i 1))
	 (j num-attrs (+ j 1)))
	((= i num-attrs))
      (let* ((xi (aref arg-array i))
	     (xj (aref arg-array j))
	     (delta (abs (- xi xj))))
	(when (/= 0 delta)
	  (incf sum-of-powers (expt delta metric-order)))))
    (expt sum-of-powers (/ 1.0 metric-order))))




