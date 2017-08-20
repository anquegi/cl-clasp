;;;; -*- Mode:Common-Lisp; Package:COMMON-LISP-USER; Fonts:(MEDFNT); Base:10 -*-
;;;; *-* File: Titanic: /usr/users/eksl/mac-files/clips/packages.lisp *-*
;;;; *-* Last-edit: Monday, January 25, 1993  15:45:21; Edited-By: Westy *-* 
;;;; *-* Machine: Count (Explorer II, Microcode 489) *-*
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

(defpackage CLASP-INTERFACE
  (:use CLIM-LISP CLIM RTM CLASP #+ignore GRAPH-USER)
  (:export CLASP)
  (:import-from CLASP .on.)
  ;;(:shadow graph)
  (:shadowing-import-from CLASP data name)
  #+LUCID
  (:shadowing-import-from COMMON-LISP-USER destructuring-bind))

(defpackage CLASP-USER
  (:use CLASP-INTERFACE CLASP RTM CLIM-LISP CLIM))

#+MCL
(import '(ccl::class-prototype
	  ccl::class-direct-superclasses
	  ccl::class-direct-subclasses
          ccl::class-precedence-list) 'CLASP-INTERFACE)

#+Allegro
(import '(clos::class-prototype 
	  clos::class-direct-superclasses 
	  clos::class-direct-subclasses
	  clos::class-precedence-list
	  clos::class-finalized-p) 'CLASP-INTERFACE)

;;; ***************************************************************************
;;; EOF



