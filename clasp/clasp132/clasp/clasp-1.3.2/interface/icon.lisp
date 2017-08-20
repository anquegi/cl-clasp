;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package:CLASP-INTERFACE -*-
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

(in-package :CLASP-INTERFACE)

(defmacro with-clasp-frame ((frame-var) &body body)
  `(let ((,frame-var (clasp-frame)))
     (when ,frame-var
       . ,body)))

;;;****************************************************************
;;; icons.lisp
;;; 7/92
;;; J. Jeffrey Close
;;;
;;; This contains code for bitmap (Clim 'pattern') icons including
;;; stationary icons, mouse-movability, command-icons (buttons),
;;; and dragged-icons (drag icon and execute function on a target)
;;;
;;; Mods:
;;;  - changed dragged-icon so target form is run in place of command
;;;    form of ocmmand-icon, rather than both running on different clicks
;;;  - there is a commented-out reference in icon initialize-instance
;;;    to a function called bitmap-to-pattern; this is a set of functions
;;;    I wrote that convert X bitmaps to Clim patterns
;;;****************************************************************

(defvar *icon-list* nil)

(defvar *icon-bitmap-directory* nil)

(clim:define-command-table icon-command-table
    :inherit-from (clim:user-command-table))

#+moved
(defclass icon (clasp::named-object)
  ((file :initform nil :initarg :file :accessor file)
   (bitmap :initform nil :initarg :bitmap :accessor bitmap)
   (presentation :initform nil :initarg :presentation :accessor presentation)
   (label-p :initform t :initarg :label-p :accessor label-p)
   (height :initform nil :initarg :height :accessor height)
   (width :initform nil :initarg :width :accessor width)
   (x :initform 0 :initarg :x :accessor x)
   (y :initform 0 :initarg :y :accessor y)
   (icon-window :initform nil :initarg :icon-window :accessor icon-window))
  )

(defmethod initialize-instance :after ((self icon) &rest ignore)
  (declare (ignore ignore))
  (setf (width self)  (clim::pattern-width (bitmap self))
	(height self) (clim::pattern-height (bitmap self)))
  (push self *icon-list*))

;;; this takes care of the filename and bitmap initialization
;;; if a filename isn't given, tries to create one from the name and
;;; looks for it; if not found, asks for one.
;;; then it loads the bitmap and sets width and height info
;;; this does NOT position the icon in a window
 
#|

(defmethod initialize-instance :after ((self icon) &rest ignore)
  (let ((filename))
  (unless (file self)
    (setq filename			;  hyphenate the name to produce file
      (substitute #\- #\space
		  (format nil "~A~A.bm" *icon-bitmap-directory* (name self))))
    (if (open filename :direction :probe)
	(setf (file self) filename)
      (progn
	(format t "Please enter a file name for the bitmap for icon ~A"
		(name self))
	(setq filename (read-line))
	;;; should check here for good input
	(setf (file self) filename))
      ))
  (when (file self)
    (multiple-value-bind (pattern wid ht)
	(bitmap-to-pattern (file self))
      (cond (pattern
	     (setf (bitmap self) pattern)
	     (setf (width self) wid)
	     (setf (height self) ht))
	    (t nil))
      ))
  (push self *icon-list*)
  ))

|#

;;; if displayed elsewhere, erases the icon, then
;;; sets the coordinates, places it icon in a window, and presents it
(defmethod place-in-window ((icon icon) window x y)
  (when (presentation icon)
    (clim:erase-output-record (presentation icon) (icon-window icon)))
  (setf (icon-window icon) window)
  (setf (x icon) x)
  (setf (y icon) y)
  (updating-output (window)
		   (clim:present icon `((,(type-of icon)))
				 :stream window
				 :view clim:+dialog-view+)))


;(clim:define-presentation-type icon ())
      
;;; present as text
(clim:define-presentation-method clim:present
    (icon (type icon) stream (view clim:textual-view) &key)
  (clim:with-text-face (:bold)
	(format stream "~A" (clasp::name icon))
	))

;;; draws the icon bitmap in its window at its coords
(defmethod draw ((self icon) &key (stream (icon-window self))
				  (x (x self))
				  (y (y self))
				  (with-label t))
  (when stream
    (clim:draw-icon* stream (bitmap self) x y)
    (clim:stream-increment-cursor-position* stream (width self) 0) 
    (when with-label
      (draw-label self :stream stream :x x :y y)))
  )

;;; erases the icon output record and clears the slots
(defmethod erase ((self icon) STREAM)
  (declare (ignore STREAM))
    (clim:erase-output-record (presentation self) (icon-window self))
    (setf (icon-window self) nil)
    (setf (presentation self) nil))

(defmethod draw-label ((self icon)
		       &key (stream (icon-window self))
			    (x (x self))
			    (y (y self))
			    (style '(:sans-serif :bold :small))
			    )
  (let* ((textx (+ x (width self) 2))
	 (texty (+ y (truncate (height self) 2)))
	 (rec (with-new-output-record (stream)
		(clim:draw-text* stream (clasp::name-string self)
				 textx texty
				 :align-x :left
				 :align-y :center
				 :text-style style
				 :ink (draw-label-ink stream)))))
    (with-bounding-rectangle* (left top right bottom) 
      rec
      (declare (ignore top bottom))
      (clim:stream-increment-cursor-position* stream (- right left) 0))))

(defmethod draw-label-ink ((stream clim::postscript-stream))
  clim:+foreground+)

(defmethod draw-label-ink ((stream t))
  clim:+flipping-ink+)

;;; present in a dialog view, rather than text
(clim:define-presentation-method clim:present
    (icon (type icon) stream (view clim:dialog-view) &key)
  (when (not (icon-window icon))
    (multiple-value-bind (x y)
	(stream-cursor-position* stream)
      (setf (icon-window icon) stream)
      (setf (x icon) x)
      (setf (y icon) y)))
  (setf (presentation icon)
    (clim:with-output-as-presentation (:object icon
					       :type (type-of icon)
					       :stream stream
					       :single-box t)
      (draw icon :stream stream))))

;;; presentation highlighting method for icons
;;; bolds the icon boundary
(clim:define-presentation-method clim:highlight-presentation ((type icon) record stream state)
  state
  (multiple-value-bind (xoff yoff)
      (clim:convert-from-relative-to-absolute-coordinates
       stream (clim:output-record-parent record))
    (clim:with-bounding-rectangle* (left top right bottom) record
	(clim::draw-rectangle* stream
			       (+ left xoff)
			       (+ top yoff)
			       (+ right xoff)
			       (+ bottom yoff)
			       :ink clim:+flipping-ink+
			       :filled nil
			       :line-thickness 3)
	)))

;;; This allows the user to drag the icon on a window with the pointer.
;;; It first puts the pointer on the icon (it's already there if this is
;;; being called from a presentation action or translator), erases the old
;;; output record, then drags the old presentation around with the pointer.
;;; The move terminates when the user releases, re-presents (sic) the icon,
;;; and sets the icon's coordinates to the pointer position
(defmethod move ((self icon) STREAM)
  (declare (ignore STREAM))
  (with-slots ((icon-window icon-window)
	       (presentation presentation)
	       (x x)
	       (y y)) self
    (when presentation
      (clim:stream-set-pointer-position* icon-window x y)
      (clim:erase-output-record presentation icon-window) ; erase the current pic
      (clim:dragging-output (icon-window t t) ; draw and drag around the icon
			    (clim:present self `((,(type-of self)))
					  :stream icon-window
					  :view clim:+dialog-view+)
			    )
      (clim:erase-output-record presentation icon-window)
      (multiple-value-bind (px py)
	  (clim:stream-pointer-position* icon-window)
	(setf x px)
	(setf y py))
      (clim:present self `((,(type-of self)))
		    :stream icon-window
		    :view clim:+dialog-view+)
      (clim:stream-finish-output icon-window)
      )))

(clim:define-command (com-move-icon
		      :menu nil
		      :command-table icon-command-table)
    ((i 'icon))
  (move i nil))

#+IGNORE
(clim:define-presentation-to-command-translator move-icon-command
    (icon com-move-icon icon-command-table
	  :gesture :select
	  :tester ((object) (eql (type-of object) 'icon))
	  :pointer-documentation
	  ((object presentation context-type frame stream)
	   (format stream "~A" (command-documentation object))
	   )
	  :documentation
	  ((object presentation context-type frame stream)
	   (format stream "~A" (command-documentation object))
	   )
	  :menu t)
  (object)
  (list object))


(defmethod cycle-highlight ((self icon) &optional (num 5) (delay 1))
  (dotimes (i num)
    (clim:set-highlighted-presentation (icon-window self) (presentation self))
    (sleep (/ delay 2.0))
    (clim:unhighlight-highlighted-presentation (icon-window self))
    (sleep (/ delay 2.0))
    ))

;;; *************************************************************************
;;; command-icons
;;; command icons are like buttons, and have a left-click function to execute
;;; buttons have a function  to execute when pressed, which takes the
;;; arguments (object frame window context)

#+moved
(defclass command-icon (icon)		; AKA a "button"
	  ((command-form :initform nil :initarg :command-form
			     :accessor command-form)
	   (command-documentation :initform (make-string 0)
			  :initarg :command-documentation
			  :accessor command-documentation)
	   ))

;(clim:define-presentation-type command-icon ())

#+ignore
(clim:define-command (com-execute-icon-command
		      :menu nil
		      :command-table icon-command-table)
    ((icon 'command-icon))
  (let ((form (command-form icon)))
    (when form
      (cond ((symbolp form)
	     (funcall form))
	    ((listp form)
	     (eval form))
	    ))
  ))

#+ignore
(clim:define-presentation-to-command-translator execute-command
    (command-icon com-execute-icon-command icon-command-table
		  :gesture :select
		  ;;;tester is needed because of translator-ordering bug in1.1
		  :tester ((object) (eql (type-of object) 'command-icon))
		  :pointer-documentation
		  ((object presentation context-type frame stream)
		   (format stream "~A" (command-documentation object))
		   )
		  :documentation
		  ((object presentation context-type frame stream)
		   (format stream "~A" (command-documentation object))
		   )
		  :menu t)
  (object presentation context-type frame event window x y)
  (list object)
  )

;;; *************************************************************************
;;; dragged-icons
;;; dragged-icons can be selected and dragged onto another part of the screen
;;; including across multiple windows or panes;

;;; if they are dragged and dropped on something, then that object is found
;;; (actually, its presentation) and the "target function" is run on
;;; that object 
;;; I've considered making them dual-function; built on command-icons, they
;;; could execute their stationary button command if not dragged

#+moved
(defclass dragged-icon (command-icon)
	  ((target :initform nil
		   :initarg :target
		   :accessor target)
	   (target-form :initform nil
			    :initarg :target-form
			    :accessor target-form)
	   ))

;(define-presentation-type dragged-icon ())

;;; the drag method allows the user to drag an icon and redraws
;;; the picture as it is tracking; it does not move the original icon,
;;; and when it is done it erases the last drag picture
;;; RETURNS the WINDOW, the TERMINATING COORDINATES where the drag terminated,
;;; and the TARGET PRESENTATION it was over when it terminated
;;; in addition, the target presentation is stored in 'target'

(defun drag-icon (icon win &rest ignore)
  (declare (ignore ignore))
  (when (presentation icon)
    (let* ((orig-win (icon-window icon))
	   (orig-x (x icon))
	   (orig-y (y icon))
	   (last-win win)
	   (last-x (x icon))
	   (last-y (y icon))
	   (rec			;(clim:with-new-output-record (win)
					;(draw icon :stream win))
	    )
	   (target)
	   #+OLD (name)
	   )
      (clim:stream-set-pointer-position* win (x icon) (y icon))
      (erase icon t)
      (setf rec (clim:with-new-output-record (win)
					     (draw icon :stream win)))
;;; Now, start dragging
      (clim:tracking-pointer
       (win :multiple-window t)
;;; null the target because we've moved off, and redraw
       (:pointer-motion (window x y)
			(setq target nil)
			(when (or (not (eql last-win window))
				  (> (abs (- x last-x)) 5)
				  (< (abs (- y last-y)) 5))
			      (clim:stream-force-output window)
			      (when rec
				    (clim:erase-output-record rec last-win))
			      (setq last-win window last-x x last-y y)
			      (setq rec (clim:with-new-output-record (window)
								     (draw icon :stream window :x x :y y)))
			      ))
	 ;;; record if we're over a presentation
       (:presentation (presentation window x y)
		      (setq target (presentation-object presentation))
					;(beep)
		      (when (or (not (eql last-win window))
				(> (abs (- x last-x)) 5)
				(< (abs (- y last-y)) 5))
			    (clim:stream-force-output window)
			    (when rec
				  (clim:erase-output-record rec last-win)
				  (setq rec nil))
			    (setq last-win window last-x x last-y y)
			    (setq rec (clim:with-new-output-record (window)
								   (draw icon :stream window :x x :y y)))
			    )
		      )
	 ;;; wrap everything up, erase the last output, and return
       (:pointer-button-press (event x y)
			      (let ((new-win (clim:event-window event)))
				(when rec
				      (clim:erase-output-record rec last-win))
				(clim:stream-finish-output last-win)
				
					; We always put it back
				(setf (target icon) target)
				(place-in-window icon orig-win orig-x orig-y)
				
				#+OLD
				(cond 
				 ;; Put icon back where it came from if we droppedit on something.
				 ((setf (target icon) target)
				  (place-in-window icon orig-win orig-x orig-y))
				 ;; Move the icon if it was not over something.
				 (t
				  (place-in-window icon new-win x y)))
				
				(clim:stream-finish-output new-win)
				(return
				 (values (clim:event-window event)
					 x y target))
				))
       )))
  )

(defmethod drag ((self dragged-icon) &rest ignore)
  (declare (ignore ignore))
  (drag-icon self (icon-window self))
  )

(clim:define-command (com-drag-icon
		      :menu nil
		      :command-table icon-command-table)
    ((icon 'dragged-icon))
  (multiple-value-bind (win x y target)
      (drag icon)
    (declare (ignore win x y))
    (let ((form (target-form icon)))
      (when (and form target)
        (etypecase form
          (symbol
           (funcall form target icon))
          (function 
           (funcall form target icon)))))))

#+ignore
(clim:define-presentation-to-command-translator drag-icon-command
    (dragged-icon com-drag-icon icon-command-table
		  :gesture :edit
		  :tester ((object) 
                           (presentation object)
                           #+OLD
                           (eql (type-of object) 'dragged-icon))
		   :pointer-documentation
		   ((object presentation context-type frame stream)
		    (declare (ignore presentation frame context-type))
		    (format stream "Drag ~a" (clasp::name object))
		    )
		   :documentation
		   ((object presentation context-type frame stream)
		    (declare (ignore presentation frame context-type))
		    (format stream "Drag ~a" (clasp::name object))
		    )
		   :menu t)
  (object)
  (list object)
  )

#+ignore
(clim:define-command (com-open-icon
		      :menu nil
		      :name "Open"
		      :command-table icon-command-table)
    ((icon 'dragged-icon))
  (open-it icon :refresh t)
  )


;;; ***********************************************************************
;;; miscellaneous icon functions

;;; returns an icon from a name
(defun find-icon (name &rest ignore)
  (declare (ignore ignore))
  (find name *icon-list* :key #'clasp::name :test #'string-equal))

;;; erase all icons from their windows
(defun erase-icons ()
  (dolist (i *icon-list*)
    (erase i nil)))

;;; erases the icon and removes it from the icon master list
(defmethod kill-icon ((self icon) &rest ignore)
  (declare (ignore ignore))
  (when (and (presentation self) (icon-window self))
    (erase self nil))
  (setq *icon-list* (remove self *icon-list*)))

;;; kills all icons on the list
(defun kill-icons ()
  (dolist (i *icon-list*)
    (kill-icon i)))

;;; just a pass-through function to a Make-instance call, 
;;; avoids hEaving to use keyword args.
#+ignore
(defmacro create-button (name &optional (documentation (string name)) command-form  &rest args)
  `(make-instance 'command-icon :name ,name
		  :command-form ,command-form
		  :command-documentation ,documentation
		  ,@args))

#+ignore
(defmacro create-drag-button (name &optional (documentation (string name)) target-form command-form  &rest args)
  `(make-instance 'dragged-icon :name ,name
		  :command-form ,command-form
		  :command-documentation ,documentation
		  :target-form ,target-form
		  ,@args))
