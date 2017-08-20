;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10; Package:CLASP-INTERFACE -*-
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
#|
To start up the frame:
   - Evaluate (CLASP-INTERFACE::clasp).

|#


(in-package :CLASP-INTERFACE)

(export '(clasp))

;;;;----------------------------------------------------------------
;;;; Frame definition
;;;;----------------------------------------------------------------

(define-gesture-name :multiple-select :button :left :shifts (:shift))
(define-gesture-name :disjoint-select :button :left :shifts (:shift :control))
(define-gesture-name :deselect        :button :left :shifts (:control))

;;;;----------------------------------------------------------------

(defvar *clasp-frame* nil)
(defvar *clasp-frame-main-window* nil)
(defvar *current-command* nil)
(defvar *current-arguments* nil)
(defvar mapping-p nil)

(defconstant *result-types*
    '(result numeric-result column number-sequence dataset
      linear-regression-verbose-result linear-regression-brief-result
      anova-one-way-table anova-one-way-result anova-two-way-table
      scheffe-table scheffe-table-entry confidence-interval-result
      significance t-test-result chi-square-result d-test-result
      graph graph-data-icon))

(defconstant *undisplayed-result-types*
    '(dataset column variable numeric-result scheffe-table scheffe-table-entry
      significance anova-one-way-table graph))

(defconstant *label-text-style*
    (make-text-style :sans-serif :bold :small))

(clim::define-application-frame clasp-frame ()
  ((datasets-and-results-display :type (member #-MCL :window :pane :none)
                                 ; 8/9/93 Since :window blows up on the MAC
				 :initform :pane
				 :accessor datasets-and-results-display)
   (datasets-window :accessor datasets-window)
   (results-window :accessor results-window)
   (inferiors :initform nil :accessor inferiors)
   (error-handling-preferences :initform nil :accessor error-handling-preferences)
   (history :initform nil))
  (:command-table (clasp
		    :inherit-from
		    (user-command-table
		     clasp-file 
                     clasp-description
		     clasp-transform clasp-tests
		     clasp-data-manipulation 
                     clasp-graphing
		     clasp-sample
		     #+ignore clasp-map
		     user-command-table
		     accept-values-pane
		     icon-command-table
		     :graph)))
  
  (:command-definer define-clasp-command*)
  (:panes ((display :application
                    :incremental-redisplay t
                    :scroll-bars :both
                    :end-of-line-action :allow)
	   (mouse-doc :pointer-documentation)
	   (menu :command-menu)
	   (interactor :interactor
		       :default-text-style (clim::make-text-style
					    :sans-serif :roman :normal))
	   (datasets :application
		     :default-text-style (clim::make-text-style
					  :sans-serif :roman :very-small)
		     :incremental-redisplay t
		     :scroll-bars :both
		     :end-of-line-action :allow
		     :end-of-page-action :allow
		     :display-function 'draw-datasets-pane)
	   (results :application
		    :default-text-style (clim::make-text-style
					 :san-serif :roman :very-small)
		    :incremental-redisplay t
		    :scroll-bars :both
		    :end-of-line-action :allow
		    :end-of-page-action :allow
		    :display-function 'draw-results-pane)
           ))
  (:layout ((datasets-and-results-in-pane
	     (:column 1
		      (menu :compute)
		      (:row :rest  
			    (:column 1/5
				     (datasets 1/2)
				     (results :rest))
			    (interactor :rest))
		      (mouse-doc :compute)))
            (datasets-and-results-not-in-pane
	     (:column 1
		      (menu :compute)
		      (interactor :rest)
		      (mouse-doc :compute)))
	    ))
  (:top-level (clasp-top-level)))	; Added for lisp-listener -AC 7/23

(defun clasp-frame ()
  *clasp-frame*)

;; This fixes the scroll bar not workig bug in MCL.
#+MCL
(defun set-scroll-max (stream)
  (ccl:set-scroll-bar-max (slot-value stream 'ccl::v-scroller) 
                          (round (* 2.5 (window-inside-height stream))))
  (ccl:set-scroll-bar-max (slot-value stream 'ccl::h-scroller) 
                          (round (* 2.5 (window-inside-width stream))))
  )

(defun stream-of (pane-name)
  (with-clasp-frame (clasp-frame)
    (get-frame-pane clasp-frame pane-name)))

(defmethod display-or-inferior-pane-p ((frame clasp-frame) window)
  (or (eq window (get-frame-pane frame 'display))
      ;; Check for killed windows at some point.
      (member window (inferiors frame) :test #'eq)))

(defmacro with-inferior-stream ((stream frame &rest options) &body body)
  `(let ((,stream (make-inferior-window ,frame . ,options)))
     . ,body))

;;; ---------------------------------------------------------------------------

(defmethod make-inferior-window ((frame clasp-frame) &rest options)
  (let ((pane (apply #'open-window-stream :parent (clasp-root)
		     :width 800 :height 400
		     :input-buffer (stream-input-buffer 
				    (get-frame-pane frame 'interactor))
               options)))
    (push pane (inferiors frame))	; remember it
    ;(window-expose pane)		; see it
    (values pane)
    ))

;;; ---------------------------------------------------------------------------

(defmacro with-auto-sizing ((stream
			     &key (max-width most-positive-fixnum)
				  (max-height most-positive-fixnum))
			    &body body)
  (let ((output-record (gensym "OUTPUT-RECORD")))
    `(let ((,output-record
	    (with-new-output-record (,stream)
	      ,@body)))
       (with-bounding-rectangle* (left top right bottom)
	 ,output-record
	 (window-set-inside-size
	  ,stream (min (+ (- right left) 5) ,max-width)
	  (min (+ (- bottom top) 5) ,max-height))
	 (window-set-viewport-position* ,stream 0 0)))))

;; Need to augment this so that different result types 
;; use different initargs for the window. Also add autosizing?
(defmethod open-it ((result result)
		    &key refresh label (auto-size t)
		    &allow-other-keys)
  (with-clasp-frame (clasp-frame)
    (let ((window (display-window result))
	  (max-width (if (typep result 'dataset)
			 500
		       most-positive-fixnum))
	  (max-height (if (or (typep result 'dataset)
			      (typep result 'column))
			  500
			most-positive-fixnum)))
      (multiple-value-bind (visible error)
	  (and window
	       (ignore-errors (window-visibility window)))
	(declare (ignore visible))
	(cond ((or error (null window))
	       (with-inferior-stream
		   (stream clasp-frame
			   :label (or label (clasp::name-string result)))
		 (setf (display-window result) stream)
		 (if auto-size
		     (with-auto-sizing (stream :max-width max-width
					       :max-height max-height)
		       (with-output-as-presentation
			   (:object result :type (type-of result)
				    :stream stream)
			   (display result stream)))
		   (with-output-as-presentation
		       (:object result :type (type-of result) :stream stream)
		     (display result stream)))
		 (window-expose stream)))
	      (t
					;(unless visible ; always expose
	       (window-expose window)	;)
	       (when refresh
		 (window-clear window)
		 (with-output-as-presentation
		     (:object result :type (type-of result) :stream window)
		   (display result window)))))))))

(defmethod close-it ((self result) &key &allow-other-keys)
  (when (display-window self)
    (setf (window-visibility (display-window self)) nil)
    (setf (display-window self) nil)))

(defmethod toggle-open-close ((self result) &rest args &key &allow-other-keys)
  (if (display-window self)
      (apply #'close-it self args)
    (apply #'open-it self args)))

(defmethod place ((result result) &optional (stream *standard-output*))
  (fresh-line stream)
  (write-string "        " stream)
  (multiple-value-bind (x y)
                       (stream-cursor-position* stream)
    (place-in-window result stream x y))
  (terpri stream))

;;; ----------------------------------------------------------------------------

(setf clasp::*dataset-class* 'dataset)

(defmethod print-object ((dataset dataset) stream)
  (print-unreadable-object (dataset stream)
    (princ (clasp::name dataset) stream)))

;(define-presentation-type dataset ())

(defmethod display ((dataset dataset) stream)
  (fresh-line stream)
  (formatting-table (stream)
    (loop for variable in (slot-value dataset 'clasp::variables) do
      ;; Column-oriented.
	(formatting-column (stream)
	   (with-output-as-presentation (:object variable
					 :type 'column
					 :stream stream)
	     (formatting-cell (stream :align-x :center)
			      (with-text-style
				  (*label-text-style* stream)
				(princ (clasp::name variable) stream))))
	   ;; Replace with `variable-values'.
	   (loop for value in (clasp::variable-value variable) do
		 (with-output-as-presentation
		     (:object (if (typep value 'number) value `',value)
		      :type (if (typep value 'number) 'number 'symbol)
		      :stream stream)
		   (formatting-cell (stream :align-x :right)
				    (princ value stream))))))))

;;; ----------------------------------------------------------------------------

;; We call variables columns because there is a 
;; symbol `variable' in the COMMON-LISP package (at least in allegro).
(setf clasp::*variable-class* 'column)

(defmethod print-object ((column column) stream)
  (print-unreadable-object (column stream)
    (princ (clasp::name column) stream)))

;(define-presentation-type column ())

(defmethod display ((variable column) stream)
  (fresh-line stream)
  (formatting-table (stream)
    (formatting-column (stream)
      (with-output-as-presentation (:object variable
					    :type 'column
					    :stream stream)
	     (formatting-cell (stream :align-x :center)
			      (with-text-style
				  (*label-text-style* stream)
				(princ (clasp::name variable) stream))))
      (loop for value in (clasp::variable-value variable) do
            (formatting-cell (stream :align-x :right)
              (princ value stream))))))

;;; ----------------------------------------------------------------------------

;(setf graph::*dataset-class* 'graph-data)

(defun make-graph-data-icon (&rest options)
  (apply #'make-instance 'graph-data-icon options))

(defun make-graph-for-dataset (the-data-icon)
  (with-slots (graph) the-data-icon
    (unless graph
      (setf graph (make-graph :x-label (x-label the-data-icon) 
			      :y-label (y-label the-data-icon)
			      :title (description the-data-icon))))
    (when (slot-boundp the-data-icon 'graph-data)
      (add-dataset graph (graph-data the-data-icon)))
    (values graph)))

(defmethod open-it ((the-data-icon graph-data-icon)
		    &key refresh label (auto-size t)
		    &allow-other-keys)
  (declare (ignore refresh label auto-size))
  (let ((graph (make-graph-for-dataset the-data-icon)))
    (open-it graph :label (clasp::name-string the-data-icon))
    (setf (display-window the-data-icon) (display-window graph))))

(defmethod display ((the-data-icon graph-data-icon) stream)
  (let ((graph (make-graph-for-dataset the-data-icon)))
    (display graph stream)))

(defmethod hardcopy ((the-data-icon graph-data-icon) filename)
  (with-open-file (file-stream filename :direction :output
		   :if-exists :overwrite
		   :if-does-not-exist :create)
    (clim:with-output-to-postscript-stream (postscript-stream file-stream :multi-page nil)
      (with-slots (graph) the-data-icon
	;; If we ever turn off auto open we will have to change this.
	(when graph
	  (graph::display-graph ;;was graph-user:: -AC
	   graph
	   :stream postscript-stream))))))

;;; ----------------------------------------------------------------------------

(setf graph::*graph-class* 'graph) ;; was graph-user:: -AC

(defmethod make-graph (&rest options)
  (apply #'make-instance graph::*graph-class* options)) ;; was graph-user:: -AC

(defmethod open-it ((graph graph)
		    &key refresh label (auto-size t)
		    &allow-other-keys)
  (declare (ignore auto-size))
  (let ((window (display-window graph)))
    (multiple-value-bind (visible error)
	(and window
	     (ignore-errors (window-visibility window)))
      (declare (ignore visible))
      (when (> (length (graph::title graph)) 30)
	(setf (graph::title graph)
	  (format nil "~a..." (subseq (graph::title graph) 0 26))))
      (when (> (length (graph::y-label graph)) 15)
	(setf (graph::y-label graph)
	  (format nil "~a..." (subseq (graph::y-label graph) 0 11))))
      (cond ((or error (null window))
	     ;; I had to hack `graph::view-graphs' to return a window here.
	     (with-clasp-frame (clasp-frame)
	       (setf (display-window graph) (graph::view-graphs
					     (list graph) 
					     :master clasp-frame
					     :title (or label (clasp::name-string graph))))))
	    (t
	     (window-expose window)
	     (when refresh (window-refresh (display-window graph))))))))


(defmethod display ((the-graph graph) stream)
  (when (> (length (graph::title the-graph)) 30)
    (setf (graph::title the-graph)
      (format nil "~a..." (subseq (graph::title the-graph) 0 26))))
  (when (> (length (graph::y-label the-graph)) 15)
    (setf (graph::y-label the-graph)
      (format nil "~a..." (subseq (graph::y-label the-graph) 0 11))))
  (graph::display-graph ;; was graph-user:: -AC
   the-graph
   ;:width 440 :height 390
   :scroll-if-necessary t
   :stream stream))

(defvar *colors* '(:gold :green :magenta :cyan :red))

(defmethod add-dataset ((graph graph) (graph-data graph:graph-data))
  ;; Pick another color if the color has already been used in this graph.
  (unless (member graph-data (graph::datasets graph) :test #'eq)
    (let* ((used-colors (mapcar #'color (graph::datasets graph)))
	   (next-color (first (set-difference *colors* used-colors :test #'eq))))
      (when (member (color graph-data) used-colors)
	(setf (color graph-data) next-color))))
  (graph::add-dataset graph graph-data))
    
(defmethod execute-on-target ((graph graph) (the-data-icon graph-data-icon))
  (let* ((new-title (concatenate 'string (graph::title graph) " and " (clasp::name-string the-data-icon)))
	 (new-graph (make-graph :x-label (x-label the-data-icon) 
				:y-label (y-label the-data-icon)
				:title new-title))
	(old-datasets (graph::datasets graph)))
    (dolist (dataset old-datasets)
      (graph::add-dataset new-graph dataset))
    ;; the-data-icon objects have a graph-data slot (if there is only
    ;; one scigraph::dataset attached to the icon) and a graph slot (which
    ;; is used for things link scatterplot w/ regression line.)  This checks
    ;; if the graph slot is being used and uses its datasets, otherwise, it
    ;; just uses the graph-data slot.
    (if (graph the-data-icon)
	(dolist (dataset (graph::datasets (graph the-data-icon)))
	  (add-dataset new-graph dataset))
	(add-dataset new-graph (graph-data the-data-icon)))
    (open-it new-graph :refresh t :label new-title)))

;;; ----------------------------------------------------------------------------

(defun redisplayable-format (stream string &rest args)
  (updating-output (stream
		    :unique-id string
		    :cache-value args
		    :cache-test #'equal
		    :copy-cache-value t)
     (apply #'format stream string args)))

(define-presentation-method present 
    (window-point (type window-point) stream (view clim:textual-view) &key)
  (redisplayable-format stream "~A (~D.~D)" 
			(window window-point)
			(point-x window-point)
			(point-y window-point)))

(defun make-window-point (window x y)
  (make-instance 'window-point :window window :x x :y y))

(define-presentation-translator screen-location 
    (blank-area 
     window-point
     clasp
     :documentation "Click to pick a location"
     :tester ((window)
	      (with-clasp-frame (clasp-frame)
		(display-or-inferior-pane-p clasp-frame window))))
  
  (window x y)
  (values (make-window-point window x y)))

;;; ----------------------------------------------------------------------------

(defmethod draw-datasets-pane ((application clasp-frame) stream)
  (dolist (dataset (clasp::find-instances 'dataset))
    (updating-output
     (stream :unique-id (clasp::name dataset)
	     :cache-value (clasp::dataset-variables dataset))
     (clim:with-text-face (:bold)
       (present dataset 'dataset :stream stream))
     (terpri stream)
     (dolist (variable (clasp::dataset-variables dataset))
       (write-char #\space stream)
       (write-char #\space stream)
       (updating-output (stream :unique-id (clasp::name variable))
			(present variable 'column :stream stream))
       (terpri stream))
     (terpri stream)))
  #+MCL
  (set-scroll-max stream))
  
(defmethod draw-datasets-window ()
  (with-clasp-frame (clasp-frame)
    (let ((stream (datasets-window clasp-frame)))
      (window-clear stream)
      (dolist (dataset (clasp::find-instances 'dataset))
	(updating-output
	 (stream :unique-id (clasp::name dataset))
	 (clim:with-text-face (:bold)
	   (present dataset 'dataset :stream stream))
	 (terpri stream)
	 (dolist (variable (clasp::dataset-variables dataset))
	   (write-char #\space stream)
	   (write-char #\space stream)
	   (updating-output (stream :unique-id (clasp::name variable))
			    (present variable 'column :stream stream))
	   (terpri stream))
	 (terpri stream)))
      #+MCL
      (set-scroll-max stream))))
  
(defmethod update-datasets-display ()
  (with-clasp-frame (clasp-frame)
    (clim:redisplay-frame-pane clasp-frame 'datasets :force-p t)
    (draw-datasets-window)))

(defmethod draw-results-pane ((application clasp-frame) stream)
  (dolist (result (clasp::find-instances 'result))
    (when (show-in-results-display result)
      (updating-output
       (stream :unique-id (clasp::name result)
	       :cache-value (clasp::name result))
       (clim:with-text-face (:bold)
	 (present result (type-of result) :stream stream))
       (terpri stream))))
  #+MCL
  (set-scroll-max stream))

(defmethod draw-results-window ()
  (with-clasp-frame (clasp-frame)
    (let ((stream (results-window clasp-frame)))
      (window-clear stream)
      (dolist (result (clasp::find-instances 'result))
	(when (show-in-results-display result)
	  (updating-output
	   (stream :unique-id (clasp::name result)
		   :cache-value (clasp::name result))
	   (clim:with-text-face (:bold)
	     (present result (type-of result) :stream stream)
	     (terpri stream)))))
      #+MCL
      (set-scroll-max stream))))
  
(defmethod update-results-display ()
  (with-clasp-frame (clasp-frame)
    (clim:redisplay-frame-pane clasp-frame 'results :force-p t)
    (draw-results-window)))

;;; ----------------------------------------------------------------------------

(defvar *command-counter* 0)

(defmethod execute-frame-command :before ((frame clasp-frame) command)
  (fresh-line)
  (format *standard-output* "~s> " (incf *command-counter*))
  (present command 'command)
  (terpri)
  (format *standard-output* "~s< " *command-counter*)
  )

(defmethod execute-frame-command :around ((frame clasp-frame) command)
  (declare (ignore command))
  (flet
      ((do-it ()
	 (if clasp::*debug*
	     (handler-case (call-next-method)
	       (clasp::clasp-error (condition)
		 (format *standard-output* "~&CLASP ERROR: ~A~%" condition))
	       #+ignore
	       (clasp::clasp-warning (condition)
		 (format *standard-output* "~&CLASP WARNING: ~A~%" condition)))
	   (handler-case (call-next-method)
	     (clasp::clasp-error (condition)
	       (format *standard-output* "~&CLASP ERROR: ~A~%" condition))
	     #+ignore
	     (clasp::clasp-warning (condition)
	       (format *standard-output* "~&CLASP WARNING: ~A~%" condition))
	     (error (condition)
	       (format *standard-output* "~&ERROR: ~A~%" condition))))))
    #-MCL (do-it)
    #+MCL (catch :cancel
	    (do-it))))

(defmethod execute-frame-command :after ((frame clasp-frame) command)
  (declare (ignore command))
  #+MCL
  (set-scroll-max (get-frame-pane frame 'display)))


;;; ----------------------------------------------------------------------------
;;; Starting up the frame

(defun clasp (&optional restart)
  (when (or restart (null *clasp-frame*))
    (setf *clasp-frame*
      (make-application-frame
       'clasp-frame
       :parent (clasp-root)
       :width 640
       :height 640)))
  (run-frame-top-level *clasp-frame*))

(defvar *clasp-root* nil)

(defun clasp-root (&optional reinit)
  (or (and (not reinit) *clasp-root*)
      (setf *clasp-root* (open-root))))
  
(defun open-root ()
  ;;thanks to Oliver Christ <oli@adler.ims.uni-stuttgart.de> for the code below
  #+Lucid (open-root-window
	   :clx
	   :host (lcl:environment-variable "DISPLAY"))
  #+Allegro-v4.1 (open-root-window
		  :clx
		  :host (system:getenv "DISPLAY"))
  #+MCL          (open-root-window :mcl)
  #+Genera       (open-root-window :sheet)
  ;;Please contact keunen@nrb.be if you modify this source code.
  #-(or Lucid Allegro-v4.1 MCL Genera)
  (warning "Unknown CLIM/LISP combination.  Please modify the
`open-root' function to your needs."))


;;;;----------------------------------------------------------------
;;;; For easy access

(defmethod run-frame-top-level :before
		((the-frame clasp-frame))
  (setf *clasp-frame-main-window*
	(get-frame-pane (clasp-frame) 'interactor)))

(defmethod run-frame-top-level :around ((the-frame clasp-frame))
  (format *standard-output* "~&; Changing *package* to ~a~%" (find-package 'clasp-user))
  (let ((*package* (find-package 'clasp-user))
	(*print-length* 10)
	(*clasp-readtable* (copy-readtable)))
    (set-macro-character #\! #'clasp::clasp-variable-reader-macro
			 nil *clasp-readtable*)
    (let ((*readtable* *clasp-readtable*))
      (call-next-method))))

;;; ----------------------------------------------------------------------------
;;; ----------------------------------------------------------------------------
;;; Application building code.

#+MCL
(defun clasp-top-level-function (&optional restart)
  (ccl::%set-toplevel nil) ; set this to nil MCL will return after clasp exits
  (handler-case   
    (clasp-interface::clasp restart)
    (error (condition)
           (unless (ccl::y-or-n-dialog 
		    (format nil "Do not touch the close box.~%~
                                 Use the QUIT command to quit.~2%~
                                 Do you really want to quit?")
				       :cancel-text nil)
             (clasp-top-level-function t)))))

#+MCL
(defun build-app (&optional (name "CLASP 1.2"))
  (ccl::save-application name :toplevel-function #'clasp-top-level-function :creator "CLSP"))

;;; ----------------------------------------------------------------------------

#+Lucid
(defun clasp-top-level-function (&optional restart)
  (clasp-interface::clasp restart)
  (lcl::quit))

#+Lucid
(defun build-app (&optional (name "clasp"))
  (lcl::disksave name :full-gc t :restart-function 'clasp-top-level-function :verbose t))

;;; ----------------------------------------------------------------------------

#|  This might be necessary for lisp-listener -AC 7/23
(defmethod clasp-top-level :before
		((the-frame clasp-frame))
  (setf *clasp-frame* the-frame)
  (setf *clasp-frame-main-window*
	(get-frame-pane *clasp-frame* 'display)))

(defmethod clasp-top-level :around ((the-frame clasp-frame))
  (format *standard-output* "~&; Changing *package* to ~a~%" (find-package 'clasp-interface))
  (let ((*package* (find-package 'clasp-interface)))
    (call-next-method)))

|#
;;; ----------------------------------------------------------------------------
  
#|

(defclass graph-icon (result)
	  ((graph :initarg :graph :reader graph))
  (:default-initargs
    :bitmap graph-pattern
    :command-form #'print-args
    :command-documentation "Graph"
    :target-form #'print-args))

(defun make-graph-icon (&key graph x-label y-label title)
  (make-instance 'graph-icon
    :graph (or graph
               (make-graph
		:title (or title
			   (format nil "Graph Number ~D"
				   (incf graph::graph-number))) ;; was graph-user:: AC
		:x-label (or x-label "Fortnights")
		:y-label (or y-label "Furlongs")
		:present-self-p nil))))

(defmethod print-object ((the-graph-icon graph-icon) stream)
  (print-unreadable-object (the-graph-icon stream)
    (princ (clasp::name the-graph-icon) stream)))

;(define-presentation-type graph-icon ())

(defmethod display ((the-graph-icon graph-icon) stream)
  (graph::display-graph ;; was graph-user:: -AC
   (graph the-graph-icon)
   :width 440 :height 390
   :scroll-if-necessary t
   :stream stream))

(defmethod execute-on-target (icon-1 icon-2)
  nil)

(defmethod execute-on-target ((the-graph-icon graph-icon) (the-data-icon graph-data-icon))
  (add-dataset (graph the-graph-icon) (graph-data the-data-icon))
  (open-it the-graph-icon :refresh t))

|#
;;; ----------------------------------------------------------------------------
