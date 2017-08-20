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

(in-package :rtm)

(export '(.numberp. .symbolp. .nullp.))

(defun number-or-symbol-< (object1 &rest objects)
  (if (null objects)
      t
    (let ((object2 (car objects)))
      (cond
       ((numberp object1)
	(if (numberp object2)
	    (and (< object1 object2)
		 (or (null (cdr objects))
		     (number-or-symbol-< object2 (cdr objects))))
	  t))
       ((numberp object2)
	nil)
       (t
	(and (string< object1 object2)
	     (or (null (cdr objects))
		 (number-or-symbol-< object2 (cdr objects)))))))))

(defun number-or-symbol-= (object1 &rest objects)
  "Return t if object1 and all other objects are of the same type and equal."
  (let ((object2 (car objects)))
    (if (numberp object1)
	(if (numberp object2)
	    (and (= object1 object2)
		 (or (null (cdr objects))
		     (number-or-symbol-= object2 (cdr objects))))
	  nil)
      (if (numberp object2)
	  nil
	(and (string= object1 object2)
	     (or (null (cdr objects))
		 (number-or-symbol-= object2 (cdr objects))))))))

(defun number-or-symbol-encode (object)
  "Convert symbol into uppercase string, leave numbers alone."
  (if (numberp object)
      object
    (string-upcase (string object))))

(defun number-or-symbol-decode (encoded-object)
  "Convert string back into a symbol, or return a number."
  (if (numberp encoded-object)
      encoded-object
    (intern encoded-object)))

(defun number-or-symbol-verify (encoded-object)
  "Verify that encoded-object is a number or string."
  (or (typep encoded-object 'number)
      (typep encoded-object 'string)))

(create-domain :number-or-symbol
	       :lisp-data-type '(or number string)
	       :less-p #'number-or-symbol-<
	       :equal-p #'number-or-symbol-=
	       :encode-fn #'number-or-symbol-encode
	       :decode-fn #'number-or-symbol-decode
	       :verify-fn #'number-or-symbol-verify
	       :default-value (number-or-symbol-encode nil))

(defun query-numberp (attr-name)
  (let ((tbl-name (current-table-name))
	(attribute (current-attribute attr-name)))
    (unless attribute
      (rtm-error "Invalid query - attribute '" attr-name
		 " does not exist for table '" tbl-name ".")
      (return-from query-numberp (values nil)))
    (let* (#+ignore (domain-name (attribute-domain-name attribute))
	   #+ignore (domain (get-domain domain-name))
	   (tuple-set (tuple-set-allocate))
	   (data-array (attribute-data attribute))
	   (table (get-table tbl-name))
	   (tuple-count (table-tuple-count table))
	   table-attr-value)
      (if (and (table-indexed-attr-name-list table)
	       (< (+ tuple-count tuple-count)
		  (table-cardinality table)))
	  (let ((total-tuple-set (query-all)))
	    (do-tuple-set (row total-tuple-set)
	      (setf table-attr-value (aref data-array row))
	      (when (numberp table-attr-value)
		(tuple-set-union-element tuple-set row)))
	    (tuple-set-release total-tuple-set))
	(do-table-filled-rows (row (table-free-tuple-chain table))
	  (setf table-attr-value (aref data-array row))
	  (when (numberp table-attr-value)
	    (tuple-set-union-element tuple-set row))))
      tuple-set)))

(defun query-symbolp (attr-name)
  (let ((tbl-name (current-table-name))
	(attribute (current-attribute attr-name)))
    (unless attribute
      (rtm-error "Invalid query - attribute '" attr-name
		 " does not exist for table '" tbl-name ".")
      (return-from query-symbolp (values nil)))
    (let* (#+ignore (domain-name (attribute-domain-name attribute))
	   #+ignore (domain (get-domain domain-name))
	   (tuple-set (tuple-set-allocate))
	   (data-array (attribute-data attribute))
	   (table (get-table tbl-name))
	   (tuple-count (table-tuple-count table))
	   table-attr-value)
      (if (and (table-indexed-attr-name-list table)
	       (< (+ tuple-count tuple-count)
		  (table-cardinality table)))
	  (let ((total-tuple-set (query-all)))
	    (do-tuple-set (row total-tuple-set)
	      (setf table-attr-value (aref data-array row))
	      (when (and (stringp table-attr-value)
			 (not (string= table-attr-value "NIL")))
		(tuple-set-union-element tuple-set row)))
	    (tuple-set-release total-tuple-set))
	(do-table-filled-rows (row (table-free-tuple-chain table))
	  (setf table-attr-value (aref data-array row))
	  (when (and (stringp table-attr-value)
			 (not (string= table-attr-value "NIL")))
	    (tuple-set-union-element tuple-set row))))
      tuple-set)))

(defun query-nullp (attr-name)
  (let ((tbl-name (current-table-name))
	(attribute (current-attribute attr-name)))
    (unless attribute
      (rtm-error "Invalid query - attribute '" attr-name
		 " does not exist for table '" tbl-name ".")
      (return-from query-nullp (values nil)))
    (let* (#+ignore (domain-name (attribute-domain-name attribute))
	   #+ignore (domain (get-domain domain-name))
	   (tuple-set (tuple-set-allocate))
	   (data-array (attribute-data attribute))
	   (table (get-table tbl-name))
	   (tuple-count (table-tuple-count table))
	   table-attr-value)
      (if (and (table-indexed-attr-name-list table)
	       (< (+ tuple-count tuple-count)
		  (table-cardinality table)))
	  (let ((total-tuple-set (query-all)))
	    (do-tuple-set (row total-tuple-set)
	      (setf table-attr-value (aref data-array row))
	      (when (and (stringp table-attr-value)
			 (string= table-attr-value "NIL"))
		(tuple-set-union-element tuple-set row)))
	    (tuple-set-release total-tuple-set))
	(do-table-filled-rows (row (table-free-tuple-chain table))
	  (setf table-attr-value (aref data-array row))
	  (when (and (stringp table-attr-value)
		     (string= table-attr-value "NIL"))
	    (tuple-set-union-element tuple-set row))))
      tuple-set)))

(defmacro .numberp. (attr-name)
  "Selects rows where table attribute is numeric."
  `(query-numberp ,attr-name))

(defmacro .symbolp. (attr-name)
  "Selects rows where table attribute is symbolic."
  `(query-symbolp ,attr-name))

(defmacro .nullp. (attr-name)
  "Selects rows where table attribute is NIL."
  `(query-nullp ,attr-name))
