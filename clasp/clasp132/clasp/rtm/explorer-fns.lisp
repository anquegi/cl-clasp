;;; -*- Mode:Common-Lisp; Package:RTM; Base:10 -*-
;;;; *-* Last-edit: Monday, July 27, 1992  23:16:50; Edited-By: LOISELLE *-* 

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

#+OLD
(cl:defpackage "RTM"
  (:use "COMMON-LISP"))

(in-package "RTM")

(export '(rtm-reset rtm-cleanup load-table store-table))

;;; -- * --

#+COMMENT
(defun set-rtm-source-directory ()
  (setf rtm::*rtm-source-directory* 
        (ccl:mac-directory-namestring (ccl:choose-file-dialog
                                       :mac-file-type :TEXT
                                       :button-string "Select"))))

;;;
;;; Reset RTM functions:
;;;

(defun rtm-reset (&key (save-changes t))
  (when save-changes 
    (rtm-cleanup))
  (init-rtm))

(defun rtm-cleanup ()
  "Store all changed tables to disk."
  (when (hash-table-p *rtm-relations*)
    (maphash #'(lambda (tbl-name table)
		 (when (< 0 (table-change-count table))
		   (store-table tbl-name)))
	     *rtm-relations*))
  (values t))

;;;
;;; RTM relation (base table) disk storage and retrieval functions.
;;;

(defun load-table (table-name &key (static-p nil))
  "Loads table rows from disk file and builds necessary indexes.
If static-p is non-nil, then exact size will be allocated.  Otherwise,
table will expand by *rtm-table-expansion-factor*."
  (let ((pathname (concatenate 'string *rtm-data-directory*
			       (prin1-to-string table-name) ".TBL")))
    ;; Check for non-existing disk file error.
    (unless (directory pathname)
      (rtm-warning "Load of table \'" table-name " failed - can't find disk file.")
      (return-from load-table))
    ;; If file found, create table and load tuples.
    (rtm-notification "Loading table \'" table-name " from disk file " pathname "...")
    (with-open-file (filestream pathname :direction :input)
      ;; Last element of arg-list is table-size.  Allocate more space if not static-p.
      (let* ((reversed-arg-list (reverse (read filestream)))
	     (current-size (first reversed-arg-list))
	     (new-size (if static-p
			   current-size
			   (round (* current-size *rtm-table-expansion-factor*))))
	     (arg-list (reverse (push new-size (rest reversed-arg-list)))))
	;; Create table and check for success.
	(unless (apply #'create-table table-name arg-list)
	  (rtm-warning "Load of table \'" table-name " failed - error in table creation.")
	  (return-from load-table (values nil)))
	(when (push-table-scope table-name)
	  ;; Read all tuples and insert them into table.
	  (do ((tuple (read filestream) (read filestream nil)))
	      ((null tuple))
	    (table-insert-tuple table-name tuple))
	  ;; Unmark table as changed.
	  (table-unmark-as-changed table-name)
	  (pop-table-scope))))
    ;; Return success.
    (values t)))

(defun store-table (table-name)
  "Stores all rows of specified table or view to disk file in domain decoded attribute format."
  (let ((pathname (concatenate 'string *rtm-data-directory*
			       (prin1-to-string table-name) ".TBL"))
	(result nil))
    (if (push-table-scope table-name)
	(let* ((table (relation-structure-table (get-rel-struct (current-table-name))))
	       (current-size (count-selection :from table-name))
	       (all-attr-names nil)
	       (attribute-spec-list nil))
	  (rtm-notification "Storing table \'" table-name " to disk file " pathname "...")
	  ;; Write out table header and data tuples.
	  (with-open-file (filestream pathname :direction :output)
	    ;; Build attribute spec for each attribute (in column order).
	    (do-table-cols (column table)
	      (let* ((attr-name (table-attr-name table column))
		     (attribute-struct (current-attribute-struct attr-name))
		     (attribute-spec (list attr-name
					   (attribute-structure-domain
					     attribute-struct))))
		(push-end attr-name all-attr-names)
		(when (attribute-structure-key-p attribute-struct)
		  (push-end :key attribute-spec))
		(when (attribute-structure-index attribute-struct)
		  (push-end :index attribute-spec))
		(push-end attribute-spec attribute-spec-list)))
	    ;; Write table file header (Note: current-size is last element of header list).
	    (format filestream "~s" (list :attributes attribute-spec-list
					 :initial-size current-size))
	    ;; Write table file tuples.
	    (do-selection (:from table-name)
	      (format filestream "~%(")
	      (dolist (attr-name all-attr-names)
		(format filestream  " ~s" (attr-value attr-name)))
	      (format filestream " )")))
	  ;; Unmark table as changed and set result to indicate success.
	  (table-unmark-as-changed table-name)
	  (setf result t)
	  ;; Restore previous table scope.
	  (pop-table-scope))
	;; Otherwise, error in table or view name.
	(rtm-warning "Store of table \'" table-name " failed - table doesn't exist."))
    ;; Return t if successful or nil if error.
    (values result)))

;;; ------------------------------------
;;; MacIntosh versions

#+COMMENT
(defun load-table (&optional tbl-name &key (static-p nil))
  "Macintosh RTM table loading function.  Loads table rows from disk file and 
builds necessary indexes.  If static-p is non-nil, then exact size will be 
allocated.  Otherwise, table will expand by *rtm-table-expansion-factor*.  A 
file whose name is table-name in *rtm-data-directory* is loaded if it exists 
and has an :RTMD mac file type.  Otherwise, the user is asked to find the file 
to load as table-name."  
  (let* ((pathname (concatenate 'string *rtm-data-directory*
                                (prin1-to-string tbl-name))))
    (unless (and tbl-name (directory pathname) (eq (ccl:mac-file-type pathname) :RTMD))
      (setf pathname (ccl:choose-file-dialog
                      :directory rtm::*rtm-data-directory*
                      :mac-file-type :RTMD
                      :button-string "Load"))
      (unless tbl-name
        (setf tbl-name (intern (ccl:mac-file-namestring pathname))))
      ;; Remember location of data directory.
      (setf *rtm-data-directory* 
        (ccl:mac-directory-namestring pathname)))
    ;; Create table and load tuples.
    (rtm-notification "Loading table \'" tbl-name " from disk file " pathname "...")
    (with-open-file (filestream pathname :direction :input)
      ;; Last element of arg-list is table-size.  Allocate more space if not static-p.
      (let* ((reversed-arg-list (reverse (read filestream)))
	     (current-size (first reversed-arg-list))
	     (new-size (if static-p
			   current-size
			   (round (* current-size *rtm-table-expansion-factor*))))
	     (arg-list (reverse (push new-size (rest reversed-arg-list)))))
	;; Create table and check for success.
	(unless (apply #'create-table tbl-name arg-list)
	  (rtm-warning "Load of table \'" tbl-name " failed - error in table creation.")
	  (return-from load-table (values nil)))
	(when (push-table-scope tbl-name)
	  ;; Read all tuples and insert them into table.
	  (do ((tuple (read filestream) (read filestream nil)))
	      ((null tuple))
	    (table-insert-tuple tbl-name tuple))
	  ;; Unmark table as changed.
	  (table-unmark-as-changed tbl-name)
	  (pop-table-scope))))
    ;; Return success.
    (values t)))

#+COMMENT
(defun store-table (&optional (tbl-name nil))
  "Stores all rows of specified table or view to disk file in domain decoded 
attribute format.  If table-name is not specified, the user is asked to select a
existing table from a list, and then to specify a directory and file name.
Otherwise, a file whose name is table-name in *rtm-data-directory* is written."
  (let ((choice-list (append (rtm:rtm-tables) (rtm:rtm-views)))
        (pathname (concatenate 'string *rtm-data-directory*
                               (prin1-to-string tbl-name)))
        (save-pp *print-pretty*)
        (result nil))
    ;; Get table-name and pathname if necessary.
    (unless (and tbl-name (or (rtm-table-p tbl-name) (rtm-view-p tbl-name)))
      ;; Let user select table or view to store on disk.
      (if (null choice-list)
        (progn
          (rtm-warning "There are no tables currently defined.")
          (setf pathname nil))
        (if (setf tbl-name (first (ccl:select-item-from-list 
                                   choice-list
                                   :window-title "Select table to store"
                                   :selection-type :single)))
          ;; Let user select directory location to store table or view.
          (setf pathname (ccl:choose-new-file-dialog 
                          :directory (concatenate 'string rtm::*rtm-data-directory* 
                                                  (string tbl-name))
                          :prompt "Store table as..."
                          :button-string "Store"))
          (setf pathname nil))))
    ;; Continue if valid pathname.
    (when (and pathname (push-table-scope tbl-name))
      (let* ((table (get-table tbl-name))
             (current-size (table-tuple-count table))
             (all-attr-names nil)
             (attribute-spec-list nil))
        (setf *print-pretty* nil)
        (rtm-notification "Storing table \'" tbl-name " to disk file " pathname "...")
        ;; Write out table header and data tuples.
        (with-open-file (filestream pathname :direction :output
                                    :if-does-not-exist :create
                                    :if-exists :rename-and-delete)
          ;; Build attribute spec for each attribute (in column order).
          (do-attributes (attribute table)
            (let* ((attr-name (attribute-name attribute))
                   (attribute-spec (list attr-name (attribute-domain-name attribute))))
              (push-end attr-name all-attr-names)
              (when (attribute-key-p attribute)
                (push-end :key attribute-spec))
              (when (attribute-index attribute)
                (push-end :index attribute-spec))
              (push-end attribute-spec attribute-spec-list)))
          ;; Write table file header (Note: current-size is last element of header list).
          (format filestream "~s" (list :attributes attribute-spec-list
                                        :initial-size current-size))
          ;; Write table file tuples.
          (do-selection (:from tbl-name)
            (format filestream "~%(")
            (dolist (attr-name all-attr-names)
              (format filestream  " ~s" (attr-value attr-name)))
            (format filestream " )")))
        ;; Unmark table as changed and set result to indicate success.
        (table-unmark-as-changed tbl-name)
        (setf result t)
        ;; Restore previous table scope.
        (pop-table-scope)
        (setf *print-pretty* save-pp)
        (ccl:set-mac-file-type pathname :RTMD)))
    ;; Return t if successful or nil if error.
    (values result)))

#+COMMENT
(defun rtm-init-menu ()
  (let ((rtm-menu (make-instance 'ccl:menu :menu-title "RTM")))
    (when (ccl:find-menu "RTM")
      (ccl:menu-deinstall (ccl:find-menu "RTM")))
    (ccl:add-menu-items rtm-menu
                    (make-instance 'ccl:menu-item 
                                   :menu-item-title "Load Table..."
                                   :menu-item-action #'load-table)
                    (make-instance 'ccl:menu-item
                                   :menu-item-title "Store Table..."
                                   :menu-item-action #'store-table))
    (ccl:menu-install rtm-menu)))
