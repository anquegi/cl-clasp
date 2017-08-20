;;;; -*- Mode:Common-Lisp; Package:CLASP-INTERFACE; Base:10 -*-
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

(in-package :clasp-interface)

#+ignore
(define-command-table clasp-map)

#+ignore
(add-menu-item-to-command-table 'clasp "Map" 
                                :menu 'clasp-map
                                :errorp nil :after "Sample"
				:documentation
				"Mapped versions of other commands")

#+ignore
(define-command (com-set-cross-product-option :command-table clasp-map
					      :name t
					      :menu "Cross Product Option")
    ((cross-product? 'boolean :prompt
		     "Should mapping functions take the cross product of their arguments?"))
  (setf (map-with-cross-product (clasp-frame)) cross-product?))

#+ignore
(define-command (com-get-x-selection :command-table clasp-map
				     :name t
				     :menu "X selection")
    ((x '(sequence column)
	:prompt "Select objects to be used for X selection"))
  (setf (x-selection (clasp-frame)) x))

#+ignore
(define-command (com-get-y-selection :command-table clasp-map
				     :name t
				     :menu "Y selection")
    ((y '(sequence column) 
	:prompt "Select objects to be used for Y selection"))
  (setf (y-selection (clasp-frame)) y))

#+ignore
(define-clasp-command-with-mapping (com-plot-regressions
				    :command-table clasp-map
				    :name t)
    ((dv column :prompt "DV")
     (iv column :prompt "IV"))
  (make-regression-plot-icon iv dv))

#+ignore
(define-command (com-map-plot-regressions :command-table clasp-map
					  :name t
					  :menu "Scatter Plots w/ reg lines")
    ((dvs '(sequence column) :prompt "DVs")
     (ivs '(sequence column) :prompt "IVs")
     (overlay? 'boolean :prompt "Overlay graphs?"))
  (let* (#+ignore
	 (ivs (x-selection (clasp-frame)))
	 #+ignore
	 (dvs (y-selection (clasp-frame)))
	 (regression-plot-icons
	  (if (map-with-cross-product (clasp-frame))
	      (mapcan #'(lambda (dv)
			  (mapcar #'(lambda (iv)
				      (make-regression-plot-icon iv dv))
				  ivs))
		      dvs)
	   (mapcar #'(lambda (iv dv)
		       (make-regression-plot-icon iv dv))
		   ivs dvs))))
    (if overlay?
	(let ((new-icon (make-overlayed-graph-icon regression-plot-icons)))
	  (place new-icon)
	  (open-it (graph new-icon)))
      (dolist (regression-plot-icon regression-plot-icons)
	(place regression-plot-icon)
	(open-it regression-plot-icon :refresh t
		 :label (name regression-plot-icon))))))

#+ignore
(define-command (com-map-command :command-table clasp-map
				 :name t
				 :menu "Map command")
    ((command 'command-name :prompt " "))
  (describe command)
  (describe (find-command-from-command-line-name
	     command (frame-command-table (clasp-frame)))))

#+ignore
(define-clasp-command (com-regression-plot :command-table clasp-graphing
				     :name t
				     :menu "Regression plot")
    ((dv 'column :prompt "DV" :mapping-option :cross-product)
     (iv 'column :prompt "IV" :mapping-option :map))
  (place (make-regression-plot-icon iv dv)))

