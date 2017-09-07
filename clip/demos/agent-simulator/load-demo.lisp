;;; -*- Mode:Common-Lisp; Package:USER; Base:10 -*-
;;;; *-* Last-edit: Friday, October 22, 1993  13:08:47; Edited-By: Westy *-* 


(in-package :clip-user)

(defun do-load ()
  (flet ((do-file (name)
	    (let ((binary (make-pathname 
			    :directory `(,@(or (pathname-directory (clip-load-pathname)) '(:RELATIVE))
					   #+SBCL "bin-sbcl")
			    :defaults (merge-pathnames name (clip-load-pathname))
			    :type #+SBCL "fasl")))
	      (load binary))))
    (do-file "generic-simulator")
    (do-file "agent-simulator")
    (do-file "simple-agent-experiment")
    (do-file "super-agent-experiment")
    (do-file "agent-experiment")
    ))



