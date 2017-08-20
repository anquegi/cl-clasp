;;;; -*- Mode:Common-Lisp; Package:USER; Base:10 -*-
;;;; *-* File: Titanic: /usr/users/eksl/mac-files/clasp/clasp/development/defsys.lisp *-*
;;;; *-* Last-edit: Thursday, June 17, 1993  10:53:41; Edited-By: StAmant *-* 
;;;; *-* Machine: Lester (Explorer II, Microcode 489) *-*
;;;; *-* Software: TI Common Lisp System 6.49 *-*
;;;; *-* Lisp: TI Common Lisp System 6.49  *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *                                                                        *
;;;; *                             Clasp Defsystem                            *
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
;;;  07-07-92 File Created.  (Carlson)
;;;  02-12-92 Began integration of the various parts version 1.0 code.  (Westy)
;;;  05-19-93 Converted to use logical pathnames in Lucid.  (Westy)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;; --*--

(in-package :USER)

;;; --*--
;;; ***************************************************************************

#+(or Allegro Lucid MCL)
(assert (find-package 'portable-defsystem)
	nil
	"~%I really need the Public Domain DEFSYSTEM-Facility"
	)

(load #-lucid (#-Explorer translate-logical-pathname #+Explorer cl::translate-logical-pathname "clasp:rtm;defsys.lisp")
      #+lucid "clasp:rtm;defsys.lisp")

#+Defsystem
(portable-defsystem:defsystem clasp
    (:default-pathname "clasp:source;"
     :default-binary-pathname
     (merge-pathnames *default-binary-path* 
		      #-MCL "clasp:source;" #+MCL (translate-logical-pathname "clasp:source;"))
     :needed-systems (rtm)
     :load-before-compile (rtm))
  
 #+(or allegro lucid) ("patches")
 ("packages")
 ("rtm-patches")
 ("parameters")
 ("utilities")
 ("super-intrinsic-mixins")
 ("intrinsic-mixins")
 ("defs")
 ("vector-fns")
 ("errors") 
 ("dataset-fns")
 ("translation-fns") 
 ("manipulation-fns")
 ("density-fns") 
 ("matrix-fns") 
 ("matrices")
 ("find-critical")
 ("statistical-fns")
 ("sampling-fns")
 ("io-fns")
 ("shortcuts")
 ("reader-macro")
 ("exports")
 )

#+DEFSYSTEM
(defun compile-and-load-clasp (&rest options)
  (let ((portable-defsystem:*load-all-before-compile* t))
    (apply 'portable-defsystem:compile-system 'clasp options)
    (apply 'portable-defsystem:load-system 'clasp options)))

;;; ----------------------------------------------------------------------------

#+(and allegro (not DEFSYSTEM))
(defsystem :CLASP
    (:default-package
	:clasp
	:default-pathname "/usr/users/eksl/group-files/clasp/development/")
  (:definitions "defs"
      (:serial "errors" "utility-fns" "vector-fns" "dataset-fns"
	       "translation-fns" "manipulation-fns" "matrix-fns"
	       "matrices"
	       (:parallel "statistical-fns" "sampling-fns" "io-fns" 
			  "shortcuts" "exports"))))

;;; ----------------------------------------------------------------------------

#+EXPLORER
(defsystem CLASP
  (:pathname-default "clasp:source;")
  (:default-output-directory "clasp:binary;")
  (:not-in-disk-label)
  (:component-systems :rtm-for-editor)
  (:do-components)
  (:compile-load-modules
   ("packages" "parameters" "utilities" "super-intrinsic-mixins"
    "intrinsic-mixins" "defs" "errors" #+ignore "utility-fns" "vector-fns"
    "dataset-fns" "translation-fns" "manipulation-fns" "density-fns"
    "matrix-fns" "matrices" "find-critical" "statistical-fns"
    "sampling-fns" "io-fns" "shortcuts" "exports"))
  #+OLD
  (:default-menu-column :programs)
  #+OLD
  (:default-system-key #\Z)
  #+OLD
  (:instance-type :eval)
  #+OLD
  (:instance-finder (clasp::clasp-init))
  #+OLD
  (:instance-creator t)
  (:documentation "Common Lisp Analytical Statistics Package for the analysis of experimental data."))

;;; ***************************************************************************
;;; EOF

