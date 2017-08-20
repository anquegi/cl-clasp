;;;; -*- Mode:Common-Lisp; Package:CLASP; Base:10 -*-
;;;; *-* File: Titanic: /usr/users/eksl/group-files/clasp/clasp/development/io-fns.lisp *-*
;;;; *-* Last-edit: Sunday, March 21, 1993  14:45:45; Edited-By: Anderson *-* 
;;;; *-* Machine: Dizzy (Explorer II, Microcode 489) *-*
;;;; *-* Software: TI Common Lisp System 6.49 *-*
;;;; *-* Lisp: TI Common Lisp System 6.49  *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *                                                                        *
;;;; *                         Input/Output Functions                         *
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
;;;  06-30-92 File Created.  (Carlson)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;; --*--

(in-package :clasp)

;;; --*--
;;; ***************************************************************************


(defun load-dataset (filename &optional
			      (defaults *data-file-defaults*))
  "Reads a dataset from a file in standard CLASP format.  `filename' is a
string or pathname.  If `filename' does not exist, an error is signaled. CLASP
format files have two parts, a header and a data section, the first line of the
header is the name of the dataset, represented as a string (e.g. it must appear
in double quotes.)  The following n lines each have one string with the name of
one of the variables in the dataset.  The rest of the file is the data section.
Each line in the data section is a lisp list containing one row of data..  The
order of the data in the rows is the same as the order of the names of the
variables in the header.  If a file is loaded more than once, each successive
load will return a new dataset."
  #+lucid (declare (ignore defaults))
  (let ((header nil)
	(variable-names nil)
	(data nil)
	(line nil))
    #-lucid
    (if defaults
	(setf filename (merge-pathnames (make-pathname :name filename)
					defaults)))
    (unless (probe-file filename)
      (error 'file-does-not-exist :filename filename))
    (with-open-file (ifile filename
		     :direction :input :if-does-not-exist nil)
      (setf header (read ifile))
      (setf header (stupid-clip-header-parsing-hack (format nil "~a" header)))
      (do ((header-line (read ifile) (read ifile)))
	  ((not (stringp header-line)) (setf line header-line))
	(push header-line variable-names)
	)
      (do ((data-line line (read ifile nil 'eof)))
	  ((equal data-line 'eof))
	(push data-line data)
	)
      (make-dataset-from-rows header (reverse data) (reverse variable-names)))
    ))

(defun stupid-clip-header-parsing-hack (string
					&optional (keyword "Experiment:"))
  (let ((pos (search keyword string)))
    (when pos (setf pos (+ pos (length keyword))))
    (if pos
	(format nil "~a" (read-from-string string nil nil :start pos))
      string)))

(defun import-dataset (filename &key (separator #\,) include-labels-p)
  "Reads a dataset in from a file, where `filename' is a string or pathname.  If
`filename' doesn't exist, an error is signaled.  The data on each line in the
file must be separated by `separator' characters or whitespace.  No error
checking is done to insure that each line of the file contains the same number
of data.  If `include-labels-p' is non-nil, the first line will be used a
variable names, otherwise they will be named variable, variable-1, variable-2,
etc.  The filename will be used for the dataset name."
  (unless (probe-file filename)
    (error 'file-does-not-exist :filename filename))
  (with-open-file (ifile filename
		   :direction :input :if-does-not-exist nil)
    (let (data
	  data-line
	  datum-start-position
	  datum
	  variable-names)
      (when include-labels-p
	    (setf data-line (read-line ifile nil 'eof))
	    (when (eq data-line 'eof)
	      (error 'read-from-empty-file :filename filename))
	    (setf data-line (substitute #\space separator data-line))
	    (setf datum-start-position 0)
	    (do ()
		((>= datum-start-position (length data-line)))
	      (multiple-value-setq (datum datum-start-position)
		(read-from-string data-line nil nil
				  :start datum-start-position))
	      (push-end (string datum) variable-names)))
      (do ((input-line (read-line ifile nil 'eof) (read-line ifile nil 'eof)))
	  ((eq input-line 'eof))
	(setf data-line nil)
	(setf datum-start-position 0)
	(setf input-line (substitute #\space separator input-line))
	(do ()
	    ((>= datum-start-position (length input-line)))
	  (multiple-value-setq
	      (datum datum-start-position)
	    (read-from-string input-line nil nil
			      :start datum-start-position))
	  (push datum data-line))
	(push (reverse data-line) data))
      (setf data (reverse data))
      (unless include-labels-p
	(setf variable-names (make-list (length (car data))
					:initial-element "Variable")))
      (make-dataset-from-rows
       (pathname-name (pathname filename)) data variable-names))))

(defun save-dataset (data filename)
  "Writes a dataset out to a file in CLASP format.  `data' must be a dataset,
and filename is a string or pathname of a file.  If `filename' already exists,
an error will be signaled."
  (when (probe-file filename)
    (error 'file-already-exists :filename filename))
  (with-open-file (ofile filename :direction :output
		   :if-does-not-exist :create)
    (format ofile "~s~%" (format nil "~s" (name data)))
    (let (row-names
	  rows)
      (dolist (variable (dataset-variables data))
	(unless (eq (id variable) 'row-number)
	  (format ofile "~s~%" (format nil "~s" (name variable)))
	  (push (id variable) row-names)))
      (setf row-names (reverse row-names))
      (setf rows (dataset-to-rows data :columns row-names))
      (dolist (i rows)
	(format ofile "~a~%" i)))))

(defun export-dataset (dataset filename &key (separator #\,) include-labels-p)
  "Writes out the values in `dataset' in row-major order to the file `filename.'
Each row in `filename' will be one row of data in `dataset.' The data in a row
will be separated by the character `separator'.  If include-labels-p is non-nil,
the variable-names will be written out on the first line.  `export-dataset' will
signal an error if a file of name `filename' already exists."
  (when (probe-file filename)
    (error 'file-already-exists :filename filename))
  (with-open-file (ofile filename :direction :output
		   :if-does-not-exist :create)
    (let (row-names
	  rows
	  (format-string (concatenate 'string "~{~a~^"
				       (string separator) "~}~%")))
      (dolist (variable (dataset-variables dataset))
	(unless (eq (id variable) 'row-number)
	  (push (id variable) row-names)))
      (setf row-names (reverse row-names))
      (if include-labels-p
	  (format ofile format-string row-names))
      (setf rows (dataset-to-rows dataset :columns row-names))
      (dolist (i rows)
	(format ofile format-string i)))))

;;; ***************************************************************************
;;; EOF
