;;; -*- Mode:Common-Lisp; Base:10 -*-
;;;; *-* Last-edit: Tuesday, July 28, 1992  14:12:59; Edited-By: LOISELLE *-* 

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

(push :rtm-v2.2 *features*)

;;; -- * --
;;;
;;; There are seven source code files including this file (init-fns).
;;; They are listed below in load-rtm which you may want to modify
;;; to accomodate your directory structure, etc.  The function
;;; compile-rtm should be used to initially compile the system.
;;;

(export '(*rtm-data-directory*
	   *rtm-error-stream* *rtm-warning-stream* *rtm-notification-stream*
	   load-rtm compile-rtm init-rtm create-domain))

(defvar *rtm-error-stream* nil
  "Stream where RTM fatal error messages are sent.")
(defvar *rtm-warning-stream* nil
  "Stream where RTM warning messages are sent.")
(defvar *rtm-notification-stream* nil
  "Stream where RTM notifications are sent.")

(defvar *rtm-source-directory* "/usr/users/eksl/carlson/rtm/v2.2/"
  "Disk file directory where RTM source code subdirectories are stored.")
(defvar *rtm-data-directory* "christa:silvey.rtm.data;"
  "Disk file system directory where RTM tables are stored.")

(defun load-rtm (&key (source-p nil))
  "Load Relational Table Manager (RTM) system."
  (let ((successful nil))
    (if *rtm-source-directory*
      (let ((fn-list '("kernel-fns" "index-fns" "query-fns" "table-fns" "user-fns" "dec-fns"))
            (file-ext (if source-p ".lisp" ".fasl")))
        (format t "~%Loading RTM v2.2...")
        (dolist (fn fn-list)
          (load (pathname (concatenate 'string *rtm-source-directory* fn file-ext)) :verbose t))
        (setf successful t)
        (init-rtm))
      (format t "~%*rtm-source-directory* not set."))
    (values successful)))

(defun compile-rtm ()
  "Compile Relational Table Manager (RTM) system."
  (when (load-rtm :source-p t)
    (let ((fn-list '("kernel-fns" "index-fns" "query-fns" "table-fns" "user-fns" "dec-fns"))
          (file-ext ".lisp"))
      (format t "~%Compiling...")
      (dolist (fn fn-list)
        (compile-file (pathname (concatenate 'string *rtm-source-directory* fn file-ext))))
      (compile-file (pathname (concatenate 'string *rtm-source-directory* "init-fns" file-ext)))))
  (values))

;;;
;;; RTM system initialization functions and variables:
;;;

(defvar *rtm-relations* nil)
(defvar *rtm-relation-views* nil)
(defvar *rtm-relation-samples* nil)
(defvar *rtm-attributes* nil)
(defvar *rtm-tuple-sets* nil)
(defvar *rtm-priority-queues* nil)
(defvar *rtm-domains* nil)

(defun init-rtm ()
  "Initialize Relational Table Manager (RTM) system."
  ;; Reset RTM system data structures.
  (if (hash-table-p *rtm-relations*)
    (clrhash *rtm-relations*)
    (setf *rtm-relations* (make-hash-table :test #'equal :size 100)))
  (if (hash-table-p *rtm-relation-views*)
    (clrhash *rtm-relation-views*)
    (setf *rtm-relation-views* (make-hash-table :test #'equal :size 100)))
  (if (hash-table-p *rtm-relation-samples*)
    (clrhash *rtm-relation-samples*)
    (setf *rtm-relation-samples* (make-hash-table :test #'equal :size 100)))
  (if (hash-table-p *rtm-attributes*)
    (clrhash *rtm-attributes*)
    (setf *rtm-attributes* (make-hash-table :test #'equal :size 500)))
  (setf *rtm-tuple-sets* nil)
  (setf *rtm-priority-queues* (make-array 10 :adjustable t :fill-pointer 0))
  (init-domains)
  (values t))

;;;
;;; Domain Initialization Structures and Functions:
;;;

(defstruct domain
  (lisp-data-type nil)
  (less-p nil)
  (equal-p #'equal)
  (encode-fn nil)
  (decode-fn nil)
  (verify-fn nil)
  (default-value nil))

(defun create-domain (name &key (lisp-data-type t) (less-p nil) (equal-p nil)
                           (encode-fn nil) (decode-fn nil) (verify-fn nil)
                           (default-value nil))
  "Initializes RTM data structures to define an attribute domain."
  (setf (gethash name *rtm-domains*)
        (make-domain
         :lisp-data-type lisp-data-type
         :less-p less-p
         :equal-p equal-p
         :encode-fn encode-fn
         :decode-fn decode-fn
         :verify-fn verify-fn
         :default-value default-value))
  (values t))

(defun init-domains ()
  "Initialize RTM default domains and intern as symbols in the keyword package."
  (setf *rtm-domains* (make-hash-table :test #'equal :size 20))
  ;; Numeric Domains:
  (create-domain :fixnum
                 :lisp-data-type 'fixnum
                 :less-p #'<
                 :equal-p #'=
                 :encode-fn #'fixnum-encode
                 :verify-fn #'(lambda (val) (typep val 'fixnum))
                 :default-value (coerce 0 'fixnum))
  (create-domain :integer
                 :lisp-data-type 'integer
                 :less-p #'<
                 :equal-p #'=
                 :encode-fn #'integer-encode
                 :verify-fn #'(lambda (val) (typep val 'integer))
                 :default-value (coerce 0 'integer))
  (create-domain :float
                 :lisp-data-type 'float
                 :less-p #'<
                 :equal-p #'=
                 :encode-fn #'float-encode
                 :verify-fn #'(lambda (val) (typep val 'float))
                 :default-value (coerce 0 'float))
  (create-domain :single-float
                 :lisp-data-type 'single-float
                 :less-p #'<
                 :equal-p #'=
                 :encode-fn #'single-float-encode
                 :verify-fn #'(lambda (val) (typep val 'single-float))
                 :default-value (coerce 0 'single-float))
  (create-domain :double-float
                 :lisp-data-type 'double-float
                 :less-p #'<
                 :equal-p #'=
                 :encode-fn #'double-float-encode
                 :verify-fn #'(lambda (val) (typep val 'double-float))
                 :default-value (coerce 0 'double-float))
  (create-domain :number
                 :lisp-data-type 'number
                 :less-p #'<
                 :equal-p #'=
                 :verify-fn #'(lambda (val) (typep val 'number))
                 :default-value (coerce 0 'number))
  ;; Symbol and String Domains:
  (create-domain :symbol
                 :lisp-data-type 'string
                 :less-p #'string<
                 :equal-p #'string=
                 :encode-fn #'symbol-encode
                 :decode-fn #'symbol-decode
                 :verify-fn #'(lambda (val) (typep val 'string))
                 :default-value "NIL")
  (create-domain :string
                 :lisp-data-type 'string
                 :less-p #'string<
                 :equal-p #'string=
                 :verify-fn #'(lambda (val) (typep val 'string))
                 :default-value "")
  (create-domain :lowercase-string
                 :lisp-data-type 'string
                 :less-p #'string<
                 :equal-p #'string=
                 :encode-fn #'lowercase-string-encode
                 :verify-fn #'(lambda (val) (typep val 'string))
                 :default-value "")
  (create-domain :uppercase-string
                 :lisp-data-type 'string
                 :less-p #'string<
                 :equal-p #'string=
                 :encode-fn #'uppercase-string-encode
                 :verify-fn #'(lambda (val) (typep val 'string))
                 :default-value "")
  ;; Date Domain:
  (create-domain :date
                 :lisp-data-type 'integer
                 :less-p #'<
                 :equal-p #'=
                 :encode-fn #'date-encode
                 :decode-fn #'date-decode
                 :verify-fn #'(lambda (val) (typep val 'integer))
                 :default-value (date-encode "1/1/00"))
  ;; Ugly Domain (any Common Lisp data type allowed):
  (create-domain :ugly
                 :lisp-data-type t
                 :equal-p #'equal
                 :default-value nil))

;;;
;;; Date manipulation functions:
;;;

(defvar *days-in-month*
	(make-array 13 :element-type 'fixnum
		    :initial-contents '(0 31 28 31 30 31 30 31 31 30 31 30 31)))

(defun date-encode (date-string)
  "Converts MM/DD/YY date string into day number of century.
Returns nil if date string is invalid."
  (let* ((delim-1 (position #\/ date-string :test #'char=))
	 (month (read-from-string (subseq date-string 0 delim-1)))
	 (delim-2 (position #\/ date-string :test #'char= :start (+ 1 delim-1)))
	 (day (read-from-string (subseq date-string (+ 1 delim-1) delim-2)))
	 (year (read-from-string (subseq date-string (+ 1 delim-2))))
	 leap-days years-since-leap-year total-days)
    ;; Perform range checking for month and year, and coarse check day.
    (when (and (<= 1 month 12) (<= 1 day 31) (<= 0 year 99))
      ;; Do leap-year calculations.
      (multiple-value-setq (leap-days years-since-leap-year) (truncate year 4))
      (setf (aref *days-in-month* 2) 28)
      (if (zerop years-since-leap-year)
	  (incf (aref *days-in-month* 2))
	  (incf leap-days))
      ;; Perform fine range check on day.
      (when (<= 1 day (aref *days-in-month* month))
	(setf total-days (+ (* 365 year) leap-days))
	(dotimes (i month)
	  (incf total-days (aref *days-in-month* i)))
	(incf total-days day))
      (values total-days))))

(defun date-decode (date-integer)
  "Convert day number of century into date string MM/DD/YY."
  (let (month day year leap-days years-since-leap-year)
    (setf year (truncate (1- date-integer) 365.25))
    ;; Do leap-year calculations.
    (multiple-value-setq (leap-days years-since-leap-year) (truncate year 4))
    (setf (aref *days-in-month* 2) 28)
    (if (zerop years-since-leap-year)
	(incf (aref *days-in-month* 2))
	(incf leap-days))
    ;; Get number of days in year, and subtract off days from months array.
    (setf day (- date-integer (+ (* 365 year) leap-days)))
    (dotimes (i 12)
      (setf month (+ i 1))
      (if (> day (aref *days-in-month* month))
	  (decf day (aref *days-in-month* month))
	  (return)))
    (values (concatenate 'string (princ-to-string month) "/"
			 (princ-to-string day) "/" (princ-to-string year)))))
