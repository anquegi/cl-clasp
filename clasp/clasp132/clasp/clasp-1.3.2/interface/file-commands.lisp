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

(in-package :CLASP-INTERFACE)

;; ----------------------------------------------------------------------------
;;; The FILE menu
;; (more to come!)

(define-command-table clasp-file)

(add-menu-item-to-command-table 'clasp "File" :menu 'clasp-file :errorp nil
				:documentation "I/O and customization commands"
				:after :start)

#+ignore (define-command-table clasp-map)

#+ignore
(define-command (com-help-me :command-table clasp-file :name t
			     :menu  "Help Menu") ()) 

#+MCL
(define-presentation-type clasp-pathname () :inherit-from 'pathname :options (new-name new-type label))

#+MCL
(define-presentation-method accept ((type clasp-pathname) (stream t) (view t) &key)
  (let ((pathname (cond (new-name
                         (ccl::choose-new-file-dialog 
                          :directory (make-pathname :defaults (ccl::choose-file-default-directory) 
                                                    :name new-name
                                                    :type (or new-type "clasp"))
                          :prompt "Save as..."
                          :button-string (or label "Save")))
                        (t
                         (ccl::choose-file-dialog :button-string (or label "Open"))))))
    (replace-input stream (namestring pathname))
    (values pathname)))

(define-command (com-load-dataset
		 :command-table clasp-file :name t
		 :menu ("Load Dataset" 
			:documentation
			"Load a dataset from a CLASP format file"))
    ((pathname #-MCL 'pathname
               #+MCL '((clasp-pathname) :label "Load")
               :prompt "Pathname"))
  (present-with-update (load-dataset pathname nil) :stream *standard-output*
		       :view +dialog-view+))

(define-command (com-import-dataset
		 :command-table clasp-file :name t
		 :menu ("Import Dataset"
			:documentation
			"Load a dataset from an ascii file"))
    ((pathname #-MCL 'pathname
               #+MCL '((clasp-pathname) :label "Import")
               :prompt "Pathname")
     (separator 'character :prompt "Separator Character")
     (include-labels-p 'boolean
		       :prompt "Include Variable Names [Yes or No]"))
  (present-with-update (import-dataset pathname :separator separator
				       :include-labels-p include-labels-p)
		       :stream *standard-output* :view +dialog-view+))
	       
(define-command (com-save-dataset :command-table clasp-file :name t
				  :menu
				  ("Save Dataset"
				   :documentation
				   "Save a dataset to a CLASP format file"))
    ((dataset 'dataset :prompt "Dataset")
     (pathname #-MCL 'pathname
               #+MCL `((clasp-pathname) :new-name ,(clasp::name-string dataset) :new-type "clasp" :label "Save")
               :prompt "Pathname"))
  (save-dataset dataset pathname))

(define-command (com-export-dataset :command-table clasp-file :name t
				    :menu ("Export Dataset"
					   :documentation
					   "Save a dataset to an ascii file"))
    ((dataset 'dataset :prompt "Dataset")
     (pathname #-MCL 'pathname
               #+MCL `((clasp-pathname) :new-name ,(clasp::name-string dataset) :new-type "text" :label "Save")
               :prompt "Pathname")
     (separator 'character :prompt "Separator Character")
     (include-labels-p 'boolean
		       :prompt "Include Variable Names [Yes or No]"))
  (export-dataset dataset pathname :separator separator
		  :include-labels-p include-labels-p))

#+ignore
(define-clasp-command (com-set-default-directory :command-table clasp-file
						 :name t
						 :menu "Set Default Directory")
    ((directory 'string :prompt "Directory")
     (type 'string :prompt "Extension"))
  (setf clasp::*data-file-defaults* (make-pathname :host "CLASP-DATA"
						   :directory directory
						   :type type)))

(define-command (com-kill-everything :command-table clasp-file
				     :name "Clear All"
				     :menu ("Clear All"
					    :documentation
					    "Clear All Data"))
    ()
  (clasp::forget-instances 'dataset)
  (clasp::forget-instances 'result t)
  (dolist (result (clasp::find-instances 'result))
	  (when (and (slot-boundp result 'display-window)
		     (not (null (display-window result))))
		(setf (window-visibility (display-window result)) nil)))
  ;; Handle the clasp variables as well.
  (setf clasp::*clasp-active-datasets* nil)
  (setf clasp::*clasp-datasets* nil)
  (with-clasp-frame (clasp-frame)
    (setf (slot-value clasp-frame 'history) nil)
    (clim:redisplay-frame-panes clasp-frame :force-p t)
    (draw-datasets-window)
    (draw-results-window)
    (window-clear *clasp-frame-main-window*)))

(define-command (com-clear-desktop :command-table clasp-file
				   :name "Clear Notebook"
				   :menu ("Clear Notebook"
					  :documentation
					  "Clear the interactor pane"))
    ()
  (with-clasp-frame (clasp-frame)
    (setf (slot-value clasp-frame 'history) nil))
  (window-clear *clasp-frame-main-window*))

(defmethod hardcopy ((the-result result) filename)
  (with-open-file (file-stream filename :direction :output
		   :if-exists :overwrite
		   :if-does-not-exist :create)
    (clim:with-output-to-postscript-stream
	(postscript-stream file-stream :multi-page t)
      (display the-result postscript-stream))))

(define-clasp-command (com-save-notebook :command-table clasp-file :name t
					 :menu
					 ("Save Notebook"
					  :documentation
					  "Save the contents of the Notebook pane to a file"))
    ((pathname #-MCL 'pathname
               #+MCL `((clasp-pathname) :new-name ,(clasp::name-string result) :new-type "notebook" :label "Save")
               :prompt "Filename"))
  (with-open-file (stream pathname :direction :output
		   :if-exists :overwrite
		   :if-does-not-exist :create)
    (dump-history *clasp-frame* :stream stream)))
  
(define-clasp-command (com-print :command-table clasp-file :name t
				 :menu
				 ("Print"
				  :documentation
				  "Print a CLASP object to a postscript file"))
    ((result 'result :prompt "Print what?")
     (pathname #-MCL 'pathname
               #+MCL `((clasp-pathname) :new-name ,(clasp::name-string result) :new-type "ps" :label "Save")
               :prompt "Filename for Postscript File"))
  (hardcopy result pathname))

(defmethod clim::maybe-set-color ((stream clim::postscript-stream) (ink clim-utils::pattern))
  nil)

(define-command (com-preferences :command-table clasp-file
				 :name t
				 :menu ("Preferences"
					:documentation
					"Customize display"))
    ()
    (with-clasp-frame
     (clasp-frame)
     (let ((result-subclasses (find-subclasses (find-class 'result)))
	   display-options
	   (datasets-and-results-display (datasets-and-results-display clasp-frame))
	   (error-handling-preferences (error-handling-preferences clasp-frame))
	   (stream #+clim-1 (frame-top-level-window clasp-frame) #+clim-2 (frame-top-level-sheet clasp-frame)))
       (dolist (class result-subclasses)
	       (when (modifiable-display-option-p (class-prototype class))
		     (setf display-options
			   (acons class (display-option (class-prototype class))
				  display-options))))
       (restart-case
	(progn
	  (accepting-values
	   (stream :own-window t :label "Preferences")
	   (with-accept-help
	    ((:subhelp
	      "How should new results be displayed?"))
	    (dolist (display-option display-options)
		    (setf (cdr display-option)
			  (accept '(member :display-in-interactor
					   :display-in-window :iconify nil)
				  :stream stream
				  :prompt
				  (format nil "~@(~a~)"
					  (class-name (car display-option)))
				  :default (cdr display-option)))
		    (fresh-line stream)))
	   (terpri stream)
	   (terpri stream)
	   (with-accept-help
	    ((:subhelp "How do you want clasp to handle errors?"))
	    (setf error-handling-preferences
		  (accept (list 'subset :use-native-debugger #+(or lucid explorer) :trap-underflow-errors)
			  :stream stream
			  :prompt "Error handling"
			  :default error-handling-preferences)))
	   (terpri stream)
	   (with-accept-help
	    ((:subhelp "Where should datasets and results be displayed?"))
	    (terpri stream)
	    (setf datasets-and-results-display
		  (accept '(member #-MCL :window :pane :none) :stream stream
			  :prompt "Display datasets and results"
			  :default datasets-and-results-display)))
	   (terpri stream))
	  (dolist (display-option display-options)
		  (setf (display-option (class-prototype (car display-option)))
			(cdr display-option)))
	  (setf (error-handling-preferences clasp-frame) error-handling-preferences)
	  #+clim-1
	  (setf clasp::*debug* (member :use-native-debugger error-handling-preferences))
	  #+explorer
	  (setf ticl::zunderflow ((member :trap-underflow-errors error-handling-preferences)))
	  #+lucid
	  (when (member 'user::floating-point-underflow user::supported-floating-point-conditions)
		(if (member :trap-underflow-errors error-handling-preferences)
		    (pushnew 'user::floating-point-underflow (user::enabled-floating-point-traps))
		  (setf (user::enabled-floating-point-traps)
			(remove 'user::floating-point-underflow (user::enabled-floating-point-traps)))))
	  (setf (datasets-and-results-display clasp-frame)
		datasets-and-results-display))
	(abort ()))
       (cond
	((eq (datasets-and-results-display clasp-frame) :none)
	 (setf (window-visibility (datasets-window clasp-frame)) nil)
	 (setf (window-visibility (results-window clasp-frame)) nil)
	 (set-frame-layout clasp-frame 'datasets-and-results-not-in-pane))
	((eq (datasets-and-results-display clasp-frame) :window)
	 (setf (window-visibility (datasets-window clasp-frame)) t)
	 (setf (window-visibility (results-window clasp-frame)) t)
	 (draw-datasets-window)
	 (draw-results-window)
	 (set-frame-layout clasp-frame 'datasets-and-results-not-in-pane))
	(t
	 (setf (window-visibility (datasets-window clasp-frame)) nil)
	 (setf (window-visibility (results-window clasp-frame)) nil)
	 (set-frame-layout clasp-frame 'datasets-and-results-in-pane))))))

(define-command (com-quit-clasp :command-table clasp-file :name t
				:menu ("Quit"
				       :documentation
				       "Leave CLASP"))
    ()
  (possibly-quit-clasp))

(defmethod possibly-quit-clasp ()
  (when #+MCL (ccl:y-or-n-dialog "Really quit CLASP?" :cancel-text nil :yes-text "Quit" :no-text "Cancel")
        #-MCL t
        (with-clasp-frame (clasp-frame)
          (setf (window-visibility (datasets-window clasp-frame)) nil)
          (setf (window-visibility (results-window clasp-frame)) nil)
          (dolist (window (inferiors clasp-frame))
            (setf (window-visibility window) nil))
          (dolist (result (clasp::find-instances 'result))
            (when (and (slot-boundp result 'display-window)
		       (not (null (display-window result))))
	      (setf (window-visibility (display-window result)) nil)))
          (frame-exit clasp-frame))
        (values t)))

#+documentation
(define-clasp-command (com-print-menus :command-table clasp :name t)
    ()
  (do-command-table-inheritance
      (command-table (find-command-table 'clasp))
    (let ((name (format nil "~A" (command-table-name command-table))))
      (when (search "CLASP-" name)
	(with-open-file (file-stream (concatenate 'string name "-menu.ps")
			 :direction :output :if-exists :overwrite
			 :if-does-not-exist :create)
	  (with-output-to-postscript-stream (stream file-stream)
	    (display-command-table-menu command-table stream
					:max-width 333 :max-height 385)))))))

#+documentation
(define-clasp-command (com-print-interactor :command-table clasp :name t)
    ()
  (with-clasp-frame (clasp-frame)
    (with-open-file (file-stream "interactor-pane.ps"
		     :direction :output :if-exists :overwrite
		     :if-does-not-exist :create)
      (with-output-to-postscript-stream (stream file-stream)
	(let ((*standard-output* stream))
	  (redisplay-frame-pane clasp-frame 'interactor :force-p t))))))

;;; ----------------------------------------------------------------------------

#| 
(define-command-table clasp-show)

(add-menu-item-to-command-table 'clasp "Show" :menu 'clasp-show :errorp nil :after "Plotting")

(define-command (COM-PRINT-ALL-DATA :command-table clasp-show :name t :menu  "Show All Data") ()
  (print-all-data))

(define-command (com-print-data-multiple-columns :command-table clasp-show :name t :menu  "Show Data Multiple Columns") ()
  "Displays the values for the selected variable."
  (let ((data-set (current-data-set *application-frame*)))
		(dolist (var (get-variable :data-set data-set :get-multiple t :label "Select variables to view."))
		  (print-column-data (column-number var) :data-set data-set))))

(define-command (com-print-data-row :command-table clasp-show :name t :menu  "Show All Data by Row") ()
  "Display all values for a selected Dataset one row at a time."
  (if (#+MCL ccl::y-or-n-dialog #-MCL y-or-n-p "Show just the current dataset?")
    (let ((d-set (current-data-set *application-frame*)))
      (dotimes (i (array-dimension (data-set-data-array d-set) 0))
        (unless (print-row-data i :data-set d-set) (return))))
    (catch 'up 
      (dolist (set *data-set-list*)
        (let ((d-set (eval set)))
          (dotimes (i (array-dimension (data-set-data-array d-set) 0))
            (unless (print-row-data i :data-set d-set) 
              (throw 'up nil))))))))

(define-command (com-print-variable-column-and-data-set :command-table clasp-show :name t :menu  "Show Variable Stuff") ()
  "Display the column number and data-set title of a variable."
  (multiple-value-bind (num set)
    (column-number (caar (get-multi-set-data)))
		(format *clasp-report-stream* "~%Column number: ~d, dataset: ~a~%"
			num set)))

(define-command (print-variable-symbols-and-names :command-table clasp-show :name t :menu  "Show Variable Symbols and Names") ()
  "Display all internal symbols and variable name strings for a selected dataset, or all datasets."
  (if (#+MCL ccl::y-or-n-dialog #-MCL y-or-n-p "Show just the current dataset?")
		  (print-variable-symbol-name 
		    :data-set (current-data-set *application-frame*))
		  (print-variable-symbol-name)))

|#
;;; ----------------------------------------------------------------------------
