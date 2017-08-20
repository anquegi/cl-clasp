;;;; -*- Mode:Common-Lisp; Package:USER; Fonts:(MEDFNT); Base:10 -*-
;;;; *-* File: Titanic: /usr/users/eksl/mac-files/clasp/clasp/load-utils.lisp *-*
;;;; *-* Last-edit: Thursday, May 27, 1993  16:36:44; Edited-By: StAmant *-* 
;;;; *-* Machine: Lester (Explorer II, Microcode 489) *-*
;;;; *-* Software: TI Common Lisp System 6.49 *-*
;;;; *-* Lisp: TI Common Lisp System 6.49  *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *                                                                        *
;;;; *                           Load File for CLASP                          *
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
;;;  03-09-93 File Created.  (Westy)
;;;  05-19-93 Converted to use logical pathnames in Lucid.  (Westy)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;; --*--

(in-package :USER)

;;; --*--
;;; ***************************************************************************
#-clim
;Add CLIM feature.
(eval-when (compile load eval)
  (when (find-package 'clim) (pushnew :clim *features*)))

; 5-26-93 These will suffice for now.  (Westy)
#+(and clim (or allegro-v4.1 lcl4.0 lcl4.1))
(pushnew :clim-1 *features*)
#+(and clim (or allegro-v4.1 lcl4.0 lcl4.1 lispworks))
(pushnew :clim-1.1 *features*)

(proclaim '(special *default-binary-path*))

#-Explorer
(setf *default-binary-path* 
  #+lucid   (make-pathname :directory '(:relative "lucid-binaries"))
  #+allegro (make-pathname :directory '(:relative "allegro-binaries"
					#+(and allegro sparc) "sparc"
					#+(and allegro mips) "mips"))
  #+MCL     (make-pathname :directory '(:relative "MCL-binaries"))
  #+lispworks (make-pathname :directory '(:relative "harlequin-binaries")))

;;; ---------------------------------------------------------------------------

(defvar *last-clasp-load-pathname* nil)

(defun load-pathname ()
  (setf *last-clasp-load-pathname*
        #+lucid lcl:*source-pathname*
        #+allegro excl:*source-pathname*
        #+(or Genera Explorer) sys:fdefine-file-pathname
        #+MCL (translate-logical-pathname *load-pathname*)
        #-(or lucid allegro xerox Genera Explorer MCL) *load-pathname*))

;;; Some load time feature checking

;; Load portable defsystem
(unless (intersection `(Explorer Defsystem) *features* :test #'string-equal)
  ;; Assume it is in the same directory as this file.
	(load (make-pathname 
               ;; Do the rest later.
	       :defaults (merge-pathnames "defsystem" #-MCL (load-pathname) #+MCL (merge-pathnames *default-binary-path* (load-pathname)))
	       :type nil)))

#+(and Lispworks Defsystem)
(system::enter-new-nicknames (find-package 'defsys) '("PORTABLE-DEFSYSTEM"))

;; Fix a problem with :UNSPECIFIC devices
#+(and Lispworks Defsystem)
(lispworks:defadvice (pathname-device fix :around) (pathname)
  (let ((device (call-next-advice pathname)))
    (if (eq device :UNSPECIFIC) nil device)))

;; Load logical pathnames
(when (and (member 'lucid *features* :test #'string-equal)
	   (not (member 'logical-pathnames-mk *features*
			      :test #'string-equal)))
      (load (make-pathname 
	 :defaults (merge-pathnames "logical-pathnames" (load-pathname))
	 :type nil)))

;;; ---------------------------------------------------------------------------

(defun probe-for-directory (key)
  (when (probe-file
         #-(or Explorer lispworks MCL)
	 (make-pathname 
	  :defaults (load-pathname)
	  :name (string-downcase (symbol-name key))
	  :type nil)
	 #+lispworks
	 (make-pathname
	  :directory `(,@(pathname-directory (load-pathname))
			 ,(string-downcase (symbol-name key))))
	 #+Explorer  
	 (make-pathname
	  :directory `(,@(pathname-directory (load-pathname))
			 ,(symbol-name key))
	  :name :wild)
	 #+MCL
	 (make-pathname
          :directory `(,@(pathname-directory (load-pathname))
                         ,(string-downcase (symbol-name key)))))
    (list key)))

#| old probe-for-directory -AC 9/7/93
  (when
    #+(or allegro lucid)
    (probe-file (make-pathname 
                  :defaults (load-pathname)
                  :name (string-downcase (symbol-name key))
                  :type nil))
    #+Explorer
    (directory
      (make-pathname
        :directory `(,@(pathname-directory (load-pathname)) ,(symbol-name key))
        :name nil
        :type :wild))
    #+MCL
    (probe-file
      (make-pathname
        :directory `(,@(pathname-directory (load-pathname)) ,(string-downcase (symbol-name key)))))
    (list key)))
|#

;; This list should be ordered starting with :development and ending with the oldest release.
(defparameter *clasp-versions*
    `(,@(probe-for-directory :development)
      ,@(probe-for-directory :clasp-1.3.2)
      ,@(probe-for-directory :clasp-1.3.1)
      ,@(probe-for-directory :clasp-1.3)
      ,@(probe-for-directory :clasp-1.3-beta)
      ,@(probe-for-directory :clasp-1.3-alpha)
      ,@(probe-for-directory :clasp-1.2)
      ,@(probe-for-directory :clasp-1.2-beta)
      ,@(probe-for-directory :clasp-1.2-alpha)
      ,@(probe-for-directory :release-1.1)
      ,@(probe-for-directory :release-1.1-beta)
      ,@(probe-for-directory :release-1.1-alpha)
      ,@(probe-for-directory :release-1.0)
      ,@(probe-for-directory :development-v2)
      ,@(probe-for-directory :release-0.9)))
      
(defvar *use-clasp-development-version* nil)
(defvar *clasp-latest-released-version* nil)
(defvar *clasp-version-used* nil)
(defvar *clasp-status* nil)
(defvar *clasp-source-directory* nil)
(defvar *clasp-patch-directory* nil)

(defun set-clasp-version (&optional version force-load-translations (load-pathname *last-clasp-load-pathname*) &aux (previous-version *clasp-version-used*))
  (unless version
    (if *clasp-versions*
	(setf version (if *use-clasp-development-version* :development
			(if (= (length *clasp-versions*) 1)
			    (first *clasp-versions*)
			  (loop for v in *clasp-versions*
			      when (yes-or-no-p "Use CLASP ~a? " v) do
				(return v)
			      finally
				(format t "~&;Aborting CLASP load.")
				(return-from set-clasp-version nil)))))
      (progn (format t "~&;No directories found - Aborting CLASP load.")
	     (return-from set-clasp-version nil))))
  
  (setf *clasp-latest-released-version* (second *clasp-versions*)
        *clasp-version-used* version
        *clasp-status* (if (search "DEVELOPMENT" (string *clasp-version-used*) :test #'char=)
                         :development
                         :released)
        *clasp-source-directory* (string-downcase (string *clasp-version-used*))
        *clasp-patch-directory* (string-downcase
				 (if (eq *clasp-version-used* :development)
				     (string *clasp-latest-released-version*)
                                   (string *clasp-version-used*))))

  (when (and load-pathname (or force-load-translations (not (eq previous-version version))))
    #+(or MCL Explorer allegro (and lucid logical-pathnames-mk) lispworks)
    (load (merge-pathnames "clasp.translations" load-pathname))
    )
  #+allegro
  (when (eq *clasp-version-used* :development)
    (format t "~&;Setting up development environment~%")
    (format t ";Turning on cross-referencing.~%")
    (xref:start-xref))
  (values *clasp-version-used*))

(defun clasp-interface-defsys-file ()
  #+(or MCL allegro)
  (translate-logical-pathname "clasp:interface;defsys")
  #+lispworks
  (translate-logical-pathname "CLASP:INTERFACE;DEFSYS")
  #+lucid
  (lp:translate-logical-pathname "clasp:interface;defsys")
  #+Explorer				; do not load interface
  "clasp:source;defsys")

(defun load-clasp (&rest options)
  ;; Get the version from the user unless we already have done this.
  (when (or *clasp-version-used* (set-clasp-version))
    (load (clasp-interface-defsys-file))
    #-Explorer
    (apply #'portable-defsystem:load-system #+CLIM 'clasp-interface #-CLIM 'clasp :recurse t options)
    #+Explorer
    (apply #'make-system :CLASP :nowarn :noconfirm options)))

(defun compile-clasp (&rest options)
  ;; Get the version from the user unless we already have done this.
  (when (or *clasp-version-used* (set-clasp-version))
    (load (clasp-interface-defsys-file))
    #-Explorer
    (let ((portable-defsystem:*load-all-before-compile* t))
      (apply #'portable-defsystem:compile-system #+CLIM 'clasp-interface #-CLIM 'clasp :propagate t options))
    #+Explorer
    (apply #'make-system :CLASP :nowarn :noconfirm options)))

;;;----------------------------------------------------------------------------

(defun clasp-copyright-notice ()
  (format t 
"~2%Copyright (c) 1990 - 1993 University of Massachusetts~%~
Department of Computer and Information Science~%~
Experimental Knowledge Systems Laboratory~%~
Professor Paul Cohen, Director.~%~
All rights reserved.~2%~

Permission to use, copy, modify and distribute this software and its~%~
documentation is hereby granted without fee, provided that the above~%~
copyright notice of EKSL, this paragraph and the one following appear~%~
in all copies and in supporting documentation.~2%~
EKSL makes no representation about the suitability of this software for any~%~
purposes.  It is provided \"AS IS\", without express or implied warranties~%~
including (but not limited to) all implied warranties of merchantability~%~
and fitness for a particular purpose, and notwithstanding any other~%~
provision contained herein.  In no event shall EKSL be liable for any~%~
special, indirect or consequential damages whatsoever resulting from loss~%~
of use, data or profits, whether in an action of contract, negligence or~%~
other tortuous action, arising out of or in connection with the use or~%~
performance of this software, even if EKSL is~%~
advised of the possiblity of such damages.~2%~

For more information write to clasp-support@cs.umass.edu~2%"))

;;; ***************************************************************************
;;; EOF



