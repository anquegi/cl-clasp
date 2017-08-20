;;;; -*- Mode:Common-Lisp; Package:CL-USER; Fonts:(MEDFNT); Base:10 -*-
;;;; *-* File: Titanic: /usr/users/eksl/mac-files/clips/defsys.lisp *-*
;;;; *-* Last-edit: Thursday, January 28, 1993  19:13:02; Edited-By: Westy *-* 
;;;; *-* Machine: Count (Explorer II, Microcode 489) *-*
;;;; *-* Software: TI Common Lisp System 6.49 *-*
;;;; *-* Lisp: TI Common Lisp System 6.49  *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *                                                                        *
;;;; *                            System Definition                           *
;;;; *                                                                        *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: David L. Westbrook
;;;             Experimental Knowledege Systems Laboratory
;;;             Paul R. Cohen, Principal Investigator
;;;             David L. Westbrook, Systems Manager
;;;             David M. Hart, Laboratory Manager
;;;             Department of Computer and Information Science
;;;             University of Massachusetts
;;;             Amherst, Massachusetts 01003.
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;; Copyright (c) 1990 - 1993 University of Massachusetts
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
;;;  05-19-93 Converted to use logical pathnames in Lucid.  (Westy)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;; --*--

(in-package :USER)

;;; --*--
;;; ***************************************************************************

#+(or Allegro Lucid MCL)
(assert (find-package 'portable-defsystem) ()
	"~%I really need the Public Domain DEFSYSTEM-Facility")

(load #-lucid (#-Explorer translate-logical-pathname #+Explorer cl::translate-logical-pathname
	       "clasp:dwim;dwim-system")
      #+lucid "clasp:dwim;dwim-system")
(load #-lucid (#-Explorer translate-logical-pathname #+Explorer cl::translate-logical-pathname
	       "clasp:scigraph;scigraph-system")
      #+lucid "clasp:scigraph;scigraph-system")
(load #-lucid (#-Explorer translate-logical-pathname #+Explorer cl::translate-logical-pathname
	       "clasp:clasp;defsys")
      #+lucid "clasp:clasp;defsys")

#+Defsystem
(portable-defsystem:defsystem clasp-interface
    (:default-pathname 
     #-(or MCL lispworks)
     "clasp:interface;"
     #+(or MCL lispworks)
     (translate-logical-pathname "CLASP:INTERFACE;")
     :default-binary-pathname
     (merge-pathnames *default-binary-path* 
 		      #-(or MCL lispworks)
 		      "clasp:interface;"
		      #+(or MCL lispworks)
 		      (translate-logical-pathname "CLASP:INTERFACE;"))
     :needed-systems (scigraph clasp)
     :load-before-compile (scigraph clasp))

 ("packages")
 ("icon-defs")
 ("icon")
 ("define-icons")
 ("class-defs")
 ("utilities")
 ("frame")
 ("history")
 ("listener")
 ("lisp-presentations")
 ("display")
 ("defclaspcom")
 ("numeric-result-classes")
 ("significance-classes")
 #+MCL
 ("confidence-inter-classes")
 #-MCL
 ("confidence-interval-classes")
 ("anova-classes")
 ("regression-classes")
 ("file-commands")
 ("graph-commands")
 ("stat-commands")
 ("data-manipulation-commands")
 ("transform-commands")
 ("test-commands")
 ("sampling-commands")
 ("map-commands")
 )

#+DEFSYSTEM
(defun compile-and-load-clasp-interface (&rest options)
  (let ((portable-defsystem:*load-all-before-compile* t))
    (apply 'portable-defsystem:compile-system 'clasp-interface options)
    (apply 'portable-defsystem:load-system 'clasp-interface options)))
  
;;; ***************************************************************************
;;; EOF
