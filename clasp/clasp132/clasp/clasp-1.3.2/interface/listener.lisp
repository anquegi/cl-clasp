;;; -*- Syntax: Common-Lisp; Package: CLASP-INTERFACE; Base: 10; Mode: LISP; -*-
;;; "Copyright (c) 1990, 1991 Symbolics, Inc.  All rights reserved."

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


;; $fiHeader: listener.lisp,v 1.11 92/01/28 22:42:58 cer Exp $

(in-package :clasp-interface)

(defmethod frame-maintain-presentation-histories ((frame clasp-frame)) t)

(defmacro condition-restart-loop ((conditions description . args) &body body)
  (declare (ignore conditions))
  (let ((tag (clim-utils:gensymbol 'restart)))
    `(tagbody ,tag
       (restart-case
	   (progn ,@body)
	 (nil ()
	   :report (lambda (stream)
		     (format stream ,description ,@args))))
       (go ,tag))))

(defmacro handle-errors (&body body)
  `(if clasp::*debug*
      (handler-case ,@body
	(clasp::clasp-error (condition)
	  (format *standard-output* "~&CLASP ERROR: ~A~%" condition))
	(clasp::clasp-warning (condition)
	  (format *standard-output* "~&CLASP WARNING: ~A~%" condition)))
    (handler-case ,@body
       (clasp::clasp-error (condition)
	 (format *standard-output* "~&CLASP ERROR: ~A~%" condition))
       (clasp::clasp-warning (condition)
	 (format *standard-output* "~&CLASP WARNING: ~A~%" condition))
       (error (condition)
	 (format *standard-output* "~&ERROR: ~A~%" condition)))))

(defun clasp-top-level (frame)
  "Runs clasp with a lisp-listener interactor pane"
  (let* ((window (frame-query-io frame))
	 (command-table (frame-command-table frame))
	 (presentation-type `(command-or-form :command-table ,command-table)))
    (unless (and (slot-boundp frame 'datasets-window)
		 (not (null (datasets-window frame))))
      (setf (datasets-window frame)
	(make-inferior-window frame :name "Datasets" :label "Datasets")))
    (unless (and (slot-boundp frame 'results-window)
		 (not (null (results-window frame))))
      (setf (results-window frame)
	(make-inferior-window frame :name "Results" :label "Results")))
    #+clim-1
    (setf clasp::*debug* (member :use-native-debugger (error-handling-preferences frame)))
    #+explorer
    (setf ticl::zunderflow (not (member :trap-underflow-errors error-handling-preferences)))
    #+lucid
    (if (member :trap-underflow-errors (error-handling-preferences frame))
	(pushnew 'user::floating-point-underflow (user::enabled-floating-point-traps))
      (setf (user::enabled-floating-point-traps)
	    (remove 'user::floating-point-underflow (user::enabled-floating-point-traps))))
    #+(or allegro lucid)
    (redisplay-frame-panes frame :force-p t) ;; Correct for menus not appearing at start-up - AC
    (with-input-focus (window)
      (terpri window)
      (let* ((*standard-input* window)
	     (*standard-output* window)
	     #+Minima (*error-output* window)
	     (*query-io* window)
	     #+Minima (*debug-io* window)
	     (*package* *package*)
	     (*** nil) (** nil) (* nil)
	     (/// nil) (// nil) (/ nil)
	     (+++ nil) (++ nil) (+ nil)
	     (- nil))
	(with-command-table-keystrokes (keystrokes command-table)
	  (condition-restart-loop (#+Genera (sys:error sys:abort)
				   #-Genera (error)
				   "Restart CLASP")
	    (clasp-command-reader
	      frame command-table presentation-type
	      :keystrokes keystrokes)))))))

(defun clasp-command-reader (frame command-table presentation-type 
				     &key keystrokes (prompt "=> "))
  (clim::catch-abort-gestures ("Return to ~A command level"
			       (frame-pretty-name frame))
    ;; Eat any abort characters that might be hanging around.
    ;; We need to do this because COMMAND-OR-FORM is wierd.
    (let* ((abort-chars clim::*abort-characters*)
	   (clim::*abort-characters* nil))
      (when (member (stream-read-gesture *standard-input* :timeout 0 :peek-p t) abort-chars)
	(stream-read-gesture *standard-input* :timeout 0)))
    (fresh-line *standard-input*)
    (multiple-value-bind (command-or-form type numeric-arg)
	(block keystroke
	  (handler-bind ((clim::accelerator-gesture
			   #'(lambda (c)
			       ;; The COMMAND-OR-FORM type is peeking for the
			       ;; first character, looking for a ":", so we
			       ;; have to manually discard the accelerator
			       (stream-read-gesture *standard-input* :timeout 0)
			       (return-from keystroke
				 (values
				   (clim::accelerator-gesture-event c)
				   :keystroke
				   (clim::accelerator-gesture-numeric-argument c))))))
	    (let ((clim::*accelerator-characters* keystrokes))
	      (accept presentation-type
		      :stream *standard-input*
		      :prompt prompt :prompt-mode :raw
		      :additional-activation-characters '(#+Genera #\End)))))
      (when (eql type :keystroke)
	(let ((command (lookup-keystroke-command-item command-or-form command-table 
						      :numeric-argument numeric-arg)))
	  (unless (characterp command)
	    (when (partial-command-p command)
	      (setq command (funcall *partial-command-parser*
				     command command-table *standard-input* nil
				     :for-accelerator t)))
	    (setq command-or-form command
		  type 'command))))
      (cond ((eql type ':keystroke)
	     (beep))
	    ((null command-or-form))
	    ((eql (presentation-type-name type) 'command)
	     (terpri)
	     (handle-errors
	      (apply (command-name command-or-form)
		     (command-arguments command-or-form)))
	     (terpri))
	    (t
	     (terpri)
	     (let ((values (multiple-value-list
			    (handle-errors (eval command-or-form)))))
	       (fresh-line)
	       (dolist (value values)
		 (present value 'expression :single-box :highlighting)
		 (terpri))
	       (setq - command-or-form)
	       (shiftf +++ ++ + -)
	       (when values
		 ;; Don't change this stuff if no returned values
		 (shiftf /// // / values)
		 (shiftf *** ** * (first values)))))))))


;;; Lisp-y stuff

(defun quotify-object-if-necessary (object)
  (if (or (consp object)
	  (and (symbolp object)
	       (not (keywordp object))
	       (not (eq object nil))
	       (not (eq object t))))
      (list 'quote object)
    object))

(defun reasonable-presentation-type (object)
  (let* ((class (class-of object))
	 (class-name (class-name class)))
    (when (presentation-type-specifier-p class-name)
      ;; Don't compute precedence list if we don't need it
      (return-from reasonable-presentation-type class-name))
    (dolist (class (class-precedence-list class))
      (when (presentation-type-specifier-p (class-name class))
	(return-from reasonable-presentation-type (class-name class))))
    nil))
