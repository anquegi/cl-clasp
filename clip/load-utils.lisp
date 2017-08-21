;;;; -*- Mode:Common-Lisp; Package:USER; Fonts:(MEDFNT); Base:10 -*-
;;;; *-* File: Titanic: /usr/users/eksl/systems/clip/load-utils.lisp *-*
;;;; *-* Last-edit: Wednesday, January 5, 1994  15:45:14; Edited-By: Westy *-* 
;;;; *-* Machine: Count (Explorer II, Microcode 489) *-*
;;;; *-* Software: TI Common Lisp System 6.49 *-*
;;;; *-* Lisp: TI Common Lisp System 6.49  *-*

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *                                                                        *
;;;; *                 Load/Compile Utilities for CLIP                        *
;;;; *                                                                        *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Copyright (c) 1994 University of Massachusetts
;;; Department of Computer Science
;;; Experimental Knowledge Systems Laboratory
;;; Professor Paul Cohen, Director.
;;; All rights reserved.

;;; Permission to use, copy, modify and distribute this software and its
;;; documentation is hereby granted without fee for non-commercial uses
;;; only (not for resale), provided that the above copyright notice of EKSL,
;;; this paragraph and the one following appear in all copies and in
;;; supporting documentation.

;;; EKSL makes no representation about the suitability of this software for any
;;; purposes.  It is provided "AS IS", without express or implied warranties
;;; including (but not limited to) all implied warranties of merchantability
;;; and fitness for a particular purpose, and notwithstanding any other
;;; provision contained herein.  In no event shall EKSL be liable for any
;;; special, indirect or consequential damages whatsoever resulting from loss
;;; of use, data or profits, whether in an action of contract, negligence or
;;; other tortuous action, arising out of or in connection with the use or
;;; performance of this software, even if EKSL is advised of the possiblity
;;; of such damages.
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  03-09-93 File Created.  (Westy)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;; --*--

;; Fix up features.
#+lispworks 
(pushnew :CLTL2 *features*)

(in-package #+CLTL2 :COMMON-LISP-USER #-CLTL2 :USER)

;;; --*--
;;; ***************************************************************************

(defun clip-load-pathname ()
  #+lucid lcl:*source-pathname*
  #+allegro *load-pathname*
  #+lispworks *load-pathname*
  #+(or Genera Explorer) sys:fdefine-file-pathname
  #-(or lucid allegro xerox Genera Explorer lispworks) *load-pathname*)

;;; **** CHANGE THIS ****
#+(or lucid allegro lispworks)
(defparameter *clip-root-pathname* "/usr/users/eksl/systems/clip/")

#+(or lucid allegro lispworks)
(defparameter *extended-lisp-root-pathname*  "/usr/users/eksl/systems/extended-lisp/")

;;; **** END CHANGES ****

(defvar *last-clip-load-pathname* (clip-load-pathname))

;;; Some load time feature checking


#+(or (and (not Explorer) (not Defsystem))
      Lispworks3.2)
;; Load portable defsystem
;; Assume it is in the same directory as this file.
(load (make-pathname 
       :directory `(,@(pathname-directory (clip-load-pathname))
		      #+(and allegro sparc) "bin-allegro-sparc"
		      #+(and allegro mips)  "bin-allegro-mips")
       :defaults (merge-pathnames "defsystem" (clip-load-pathname))
       :type nil))

#+(and Lispworks3.1 Defsystem)
;; If CLIM has been loaded the Defsystem has been loaded, but the package
;; name is not correct. So we fix it up.
(system::enter-new-nicknames (find-package 'defsys) '("PORTABLE-DEFSYSTEM"))

;; Fix a problem with :UNSPECIFIC devices
#+(and Lispworks3.1 Defsystem)
(lispworks:defadvice (pathname-device fix :around) (pathname)
  (let ((device (call-next-advice pathname)))
    (if (eq device :UNSPECIFIC) nil device)))

;;; ---------------------------------------------------------------------------
  
(defun clip-probe-for-directory (key)
  (when (probe-file
         #+(or Lucid Allegro)
	 (make-pathname 
	  :defaults (clip-load-pathname)
	  :name (string-downcase (symbol-name key))
	  :type nil)
	 #+lispworks
	 (make-pathname
	  :directory `(,@(pathname-directory (clip-load-pathname))
			 ,(string-downcase (symbol-name key))))
	 #+Explorer  
	 (make-pathname
	  :directory `(,@(pathname-directory (clip-load-pathname))
			 ,(symbol-name key))
	  :name :wild)
         #+MCL
         (make-pathname
          :directory `(,@(pathname-directory (clip-load-pathname))
                         ,(string-downcase (symbol-name key))))
	 #-(or Lucid  Allegro lispworks Explorer MCL)
	 (error "Do not know how to probe for a directory in this lisp implementation")
	 )
    (list key)))

;; This list should be ordered starting with :development and ending with the oldest release.
(defparameter *clip-versions*
  `(,@(clip-probe-for-directory :development)
    ,@(clip-probe-for-directory :clip-2.0)
    ,@(clip-probe-for-directory :clip-1.4)	
    #+Lispworks ;; only checked it on lispworks
    ,@(clip-probe-for-directory :clip-1.3.2)	
    #+(or Explorer Lispworks)
    ,@(clip-probe-for-directory :clip-1.3.1)	
    ,@(clip-probe-for-directory :clip-1.3)	
    #+Lispworks ;; only checked it on lispworks
    ,@(clip-probe-for-directory :clip-1.2.4)
    ,@(clip-probe-for-directory :clip-1.2.2)	
    ,@(clip-probe-for-directory :clip-1.2.1)	
    ,@(clip-probe-for-directory :clip-1.2)
    ,@(clip-probe-for-directory :clip-1.1.1)
    ,@(clip-probe-for-directory :clip-1.1)
    ,@(clip-probe-for-directory :release-1.0)	
    ,@(clip-probe-for-directory :release-0.9)))

(defvar *clip-version-to-use* nil)
(defvar *clip-latest-released-version* nil)
(defvar *clip-version-used* nil)
(defvar *clip-status* nil)
(defvar *clip-source-directory* nil)
(defvar *clip-patch-directory* nil)

(defun set-clip-version (&optional (version *clip-version-to-use*)
			 force-load-translations (load-pathname *last-clip-load-pathname*) 
			 &aux (previous-version *clip-version-used*))
  (unless version
    (setf version (if (= (length *clip-versions*) 1)
                      (first *clip-versions*)
		      (loop for v in *clip-versions*
                            when (yes-or-no-p #+Lispworks "Use CLIP ~a? (yes or no) " #-Lispworks "Use CLIP ~a? " v) do
			    (return v)
                            finally
			    (format t "~&;Aborting CLIP load.")
			    (throw 'abort-load-clip t)))))
  (setf *clip-latest-released-version* (first (last *clip-versions*))
        *clip-version-used* version
        *clip-status* (if (search "DEVELOPMENT" (string *clip-version-used*) :test #'char=)
                         :development
                         :released)
        *clip-source-directory* (#-Explorer string-downcase #+Explorer identity
                                 (string *clip-version-used*))
        *clip-patch-directory* (#-Explorer string-downcase #+Explorer identity
				 (if (eq *clip-version-used* :development)
				     (string *clip-latest-released-version*)
                                   (string *clip-version-used*))))

  (when (or force-load-translations (not (eq previous-version version)))
    #+MCL 
    (load-logical-pathname-translations "CLIP")
    #+(or Explorer allegro (and lucid logical-pathnames-mk) lispworks)
    (load (merge-pathnames "clip.translations" load-pathname)))
  (values *clip-version-used*))

(defun clip-defsys-file ()
  #+(or MCL Explorer)
  "CLIP:source;defsys"
  #+allegro
  (translate-logical-pathname "clip:source;defsys")
  #+(or lucid lispworks)
  (make-pathname
   :directory `(,@(pathname-directory (clip-load-pathname))
		  ,*clip-source-directory*)
   :name "defsys"
   :type "lisp"))

(defun load-clip (&rest options)
  (catch 'abort-load-clip
    ;; Get the version from the user unless we already have done this.
    (unless *clip-version-used* (set-clip-version))
    (load (clip-defsys-file))
    #-Explorer
    (apply #'portable-defsystem:load-system 'clip :recurse t options)
    #+Explorer
    (apply #'make-system :CLIP :nowarn :noconfirm options)
    (unless #+Explorer (member :NO-LOAD-PATCHES options)
            #-Explorer (getf options :NO-LOAD-PATCHES)
      (load-clip-patches t))
    ))

(defun compile-clip (&rest options)
  (catch 'abort-load-clip
    ;; Get the version from the user unless we already have done this.
    (unless *clip-version-used* (set-clip-version))
    (load (clip-defsys-file))
    #-Explorer
    (let ((portable-defsystem:*load-all-before-compile* t))
      (apply #'portable-defsystem:compile-system 'clip :propagate t options))
    #+Explorer
    (apply #'make-system :CLIP :compile :nowarn :noconfirm options)
    (unless #+Explorer (member :NO-LOAD-PATCHES options)
            #-Explorer (getf options :NO-LOAD-PATCHES)
      (load-clip-patches t))
    ))

;;;----------------------------------------------------------------------------

(defun clip-copyright-notice ()
  (format t 
"~2%Copyright (c) 1994 University of Massachusetts~%~
Department of Computer Science~%~
Experimental Knowledge Systems Laboratory~%~
Professor Paul Cohen, Director.~%~
All rights reserved.~2%~
Permission to use, copy, modify and distribute this software and its~%~
documentation is hereby granted without fee for non-commercial uses~%~
only (not for resale), provided that the above copyright notice of EKSL,~%~
this paragraph and the one following appear in all copies and in~%~
supporting documentation.~2%~
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



;;;----------------------------------------------------------------------------

(defun load-clip-patches (&optional verbose &aux (first-time t))
  (dolist (file 
	    #+(or Explorer Lispworks Lucid)
	    (#+Explorer reverse
             #-Explorer progn
	     (directory
	       (make-pathname
		 #+Explorer :host #+Explorer (pathname-host *last-clip-load-pathname*)
		 :directory `(,@(pathname-directory *last-clip-load-pathname*)
			      #+Explorer "PATCHES" #-Explorer "patches" ,*clip-patch-directory*)
		 :name :wild
		 :type #+Explorer "XLD"
		 #+(and Lispworks Alpha) "afasl"
		 #+(and Lispworks Sun4) "wfasl"
		 #+(and Lucid Sparc)   "sbin"
		 #+(and Lucid Mips)    "mbin")))
	    #-(or Explorer Lispworks Lucid)
	    nil)
    (when (and verbose first-time)
      (format *standard-output* "~&; Loading patches for ~a~%"  *clip-version-used*)
      (setf first-time nil))
    (load file :verbose verbose)))

;;; ***************************************************************************
;;; EOF
