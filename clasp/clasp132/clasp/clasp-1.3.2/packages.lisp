;;;; -*- Mode:Common-Lisp; Package:COMMON-LISP-USER; Fonts:(MEDFNT); Base:10 -*-
;;;; *-* File: Titanic: /usr/users/eksl/group-files/clasp/clasp/development/packages.lisp *-*
;;;; *-* Last-edit: Thursday, May 13, 1993  11:37:43; Edited-By: Anderson *-* 
;;;; *-* Machine: Chuck (Explorer II, Microcode 489) *-*
;;;; *-* Software: TI Common Lisp System 6.49 *-*
;;;; *-* Lisp: TI Common Lisp System 6.49  *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *                                                                        *
;;;; *                           Package Definitions                          *
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

(in-package :USER)

;;; --*--
;;; ***************************************************************************
 
(defpackage MATRICES)

(defpackage CLASP
  (:use #+(or allegro Explorer lcl4.0) CLOS 
	#+Explorer CL
	RTM
	#+(or (not lucid) (and lucid ansi-packages))
	COMMON-LISP
	#+(and lucid (not ansi-packages))
	LISP
	MATRICES)
  #+(or LCL4.0 LCL4.1)
  (:import-from lucid-common-lisp define-condition)
  #+(or LCL4.0 LCL4.1)
  (:import-from clos class-finalized-p class-precedence-list
		     class-direct-superclasses class-direct-subclasses)
  #+(or LCL4.0 LCL4.1)
  (:import-from lucid load-time-value)
  #+lucid
  (:import-from lucid destructuring-bind print-unreadable-object row-major-aref)
  #+MCL
  (:import-from ccl class-prototype class-direct-superclasses
		class-direct-subclasses class-precedence-list)
  #+Allegro
  (:import-from clos class-prototype class-direct-superclasses 
		class-direct-subclasses class-precedence-list
		class-finalized-p)
  ;; The TI doesn't have :import-from, so we use qualified names.  This will be okay on
  ;; non-Explorers because read-time conditionals bind *read-suppress*  SDA 5/13/93
  #+EXPLORER
  (:import cl:define-condition ticl:function-name ticl:push-end))

;;; ***************************************************************************
;;; EOF



