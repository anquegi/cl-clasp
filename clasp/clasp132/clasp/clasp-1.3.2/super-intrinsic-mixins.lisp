;;;; -*- Mode:Common-Lisp; Package:CLASP; Fonts:(MEDFNT); Base:10 -*-
;;;; *-* File: Titanic: /usr/users/eksl/group-files/clasp/v3dev/super-intrinsic-mixins.lisp *-*
;;;; *-* Last-edit: Thursday, February 25, 1993  12:13:04; Edited-By: carlson *-* 
;;;; *-* Machine: Miles (Explorer II, Microcode 489) *-*
;;;; *-* Software: TI Common Lisp System 6.49 *-*
;;;; *-* Lisp: TI Common Lisp System 6.49  *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *                                                                        *
;;;; *                          Metaclass definitions                         *
;;;; *                                                                        *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: David L. Westbrook
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
;;;  01-19-93 File Created.  (Westy)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;; --*--

(in-package :CLASP)

;;; --*--
;;; ***************************************************************************

;; define a metaclass that automatically defines a method for accessing
;; objects by their name.

(defclass our-standard-class (standard-class)
  ())

(defvar *all-class-names* nil)

(defmethod initialize-instance :after ((the-class our-standard-class) &key)
  (pushnew (class-name the-class) *all-class-names*))

(defclass named-class (our-standard-class)
  ())

(defmethod initialize-instance :after ((the-named-class named-class) &key)
  (eval `(defmethod ,(class-name the-named-class) ((name symbol))
	   (get name ',(class-name the-named-class)))))

;; An alternative to `named-class' that can be used to build classes
;; that can be mixed in with classes built with `named-class'.

(defclass basic-class (our-standard-class)
  ())

;; This indicates that it is OK to build a class that has `named-class' as its
;; meta-class and includes classes built on `basic-class' in its superclasses.

(defmethod validate-superclass
	   ((class-prototype named-class) (superclass basic-class))
  t)

(defmethod validate-superclass
	   ((class-prototype named-class) (superclass standard-class))
  t)

;;; ***************************************************************************
;;; EOF
