;;; -*- Mode:Common-Lisp; Package:COMMON-LISP-USER; Base:10 -*-
;;;; *-* Last-edit: Monday, August 9, 1993  20:23:47; Edited-By: File Server *-* 

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
;;;
;;;  05-19-93 Converted to use logical pathnames in Lucid.  (Westy)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;; --*--

(in-package :USER)

;;; --*--
;;; ***************************************************************************

#+(or Allegro Lucid MCL)
(assert (find-package 'portable-defsystem) ()
  "~%I really need the Public Domain DEFSYSTEM-Facility in Package MAKE"
  )

#+Defsystem
(portable-defsystem:defsystem rtm
    (:default-pathname #-lucid (translate-logical-pathname "clasp:rtm;")
                       #+lucid "clasp:rtm;"
     :default-binary-pathname 
     (merge-pathnames *default-binary-path*
		      #-(or MCL lispworks)
                      "clasp:rtm;"
                      #+(or MCL lispworks)
                      (translate-logical-pathname "clasp:rtm;")))
  ("packages")
  ("init-fns")
  ("kernel-fns")
  ("index-fns")
  ("query-fns")
  ("table-fns") 
  ("user-fns")
  #+explorer
  ("explorer-fns")
  #+dec3100
  ("dec-fns")
  #+sun
  ("sun-fns")
  ("last-file"))

#+DEFSYSTEM
(defun compile-and-load-rtm (&rest options)
  (let ((portable-defsystem:*load-all-before-compile* t))
    (apply 'portable-defsystem:compile-system 'rtm options)
    (apply 'portable-defsystem:load-system 'rtm options)))

;;; ----------------------------------------------------------------------------

#+Explorer
(defparameter *load-pathname* (cl::translate-logical-pathname sys::fdefine-file-pathname))

#+Explorer
(fs:add-logical-pathname-host
  "RTM-FOR-EDITOR" (pathname-host *load-pathname*)
  `(("source"       "" ,(pathname-directory *load-pathname*))
    
    ("binary"       "" ,(append (pathname-directory *load-pathname*)
				`(,"explorer-binaries")))))

#+EXPLORER
(ticl:defsystem RTM-FOR-EDITOR
  (:pathname-default "RTM-FOR-EDITOR:SOURCE;")
  (:default-output-directory "RTM-FOR-EDITOR:BINARY;")
  (:not-in-disk-label)
  (:do-components)
  (:compile-load-modules
    ("packages"
     "init-fns"
     "kernel-fns"
     "index-fns"
     "query-fns"
     "table-fns"
     "user-fns"
     #+explorer
     ("explorer-fns")
     #+dec3100
     ("dec-fns")
     #+sun
     ("sun-fns")
     "last-file")))

;;;----------------------------------------------------------------------------
;;; EOF





