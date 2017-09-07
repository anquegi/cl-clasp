(in-package :cl-user)
(defpackage clip-test
  (:use :cl
        :clip
        :prove))
(in-package :clip-test)

(setf prove:*enable-colors* nil)

(plan 3)

(format t "generic-simulator~%~%")

(defvar *current-time* 0)
(defvar *event-queue* nil)
(defvar *stop-simulation?* nil)

(defstruct (event (:conc-name event.) (:type list))
  time
  period
  function
  args)

(defun initialize-simulation ()
  "Reset the simulator to an initial state."
  (setf *stop-simulation?* nil)
  (setf *event-queue* nil)
  (setf *current-time* 0))

(defun simulate (&optional (finish-time most-positive-fixnum))
  "Run the simulator."
  (loop until (or *stop-simulation?*
                  (>= *current-time* finish-time)
                  (null *event-queue*))
        for event = (pop *event-queue*)
        do 
        (setf *current-time* (event.time event))
        (run-event event)))
        
(defun run-event (event)
  (apply (event.function event) (event.args event))
  (when (event.period event)
    (schedule-event (event.function event) 
                    (event.args event) 
                    (+ (event.time event) (event.period event))
                    (event.period event))))

(defun schedule-event (function args time &optional period)
  "Schedule `function' to run at `time'.  If period is specified the function will be
rescheduled each time after it has run."

  (el::pushnew-ordered  
   (make-event :time time
               :function function
               :args args
               :period period)
   *event-queue*
   #'<
   :test #'eq
   :key #'event.time))

(defun unschedule-event (event)
  (when (find event *event-queue* :test #'eq)
    (setf *event-queue* (delete event *event-queue* :test #'eq)))
  ;; handle the case where a periodic event is calling unschedule as part
  ;; of its event.function code
  (setf (event.period event) nil))

(defun stop-simulation ()
  (setf *stop-simulation?* t))

(defparameter *notes* nil)

(defun current-time ()
  *current-time*)

(defun test-simulator ()
  (initialize-simulation)
  (schedule-event
   #'(lambda ()
       (push *current-time* *notes*))
   nil
   0
   100)
  (simulate 1000))

;; End sample demo generic-simulator

(test-simulator)

(is (length *notes*) 11)
(is (first *notes*) 1000)
(is (car (last *notes*)) 0)

(finalize)

(format t "agent-simulator~%~%")
;;;----------------------------------------------------------------------------
;;; The agent class

(defclass agent (clip::named-object-mixin clip::remember-instances clip::object-with-properties)
  ((state :initform 'state1 :accessor state)
   (cost :initform 0 :accessor cost))
  (:metaclass clip::named-class))

;;;----------------------------------------------------------------------------
;;; Agent methods

(defun change-of-state-event-function (the-agent new-state)
  (when *verbose*
    (format *standard-output* "~&~d ~a: ~a~%"
	    *current-time* (clip::name-string the-agent) new-state))
  (values new-state the-agent))

(defmethod (setf state) :after (new-value (the-agent agent))
  (change-of-state-event-function the-agent new-value))

(defvar *verbose* nil)

;;;----------------------------------------------------------------------------
;;; States

(defvar *state-list* '(state1 state2 state3 state4 state5 state6))

(defun state-p (state)
  (when (member state *state-list*)
    (values t)))

(deftype state ()
  '(satisfies state-p))

(defun highest-state-p (state)
  (check-type state state)
  (eq state (first (last *state-list*))))

(defun state< (statex statey)
  (check-type statex state)
  (check-type statey state)
  (when (member statey (rest (member statex *state-list*)))
    (values t)))

(defun next-state (state)
  (check-type state state)
  (first (rest (member state *state-list*))))

;;;----------------------------------------------------------------------------
;;; The agents execution function

(defparameter *transition-probability* .01)
(defparameter *relative-cost* 1)

(defmethod run-agent ((the-agent agent))
  (with-accessors  ((state state) (cost cost)) the-agent
    (unless (highest-state-p state)
      ;; Possibly bop it up to the next state.
      (when (probability-p *transition-probability*)
	(setf state (next-state state)))
      ;; When they have reached the highest state incur a cost.
      (when (highest-state-p state)
	(setf cost (* *current-time* *relative-cost*))))))

;;;----------------------------------------------------------------------------
;;; Simulation running

(defun reset-agent-simulation ()
  (clip::forget-instances 'agent)
  (initialize-simulation))
      
(defun run-agent-simulation (&key (reset t) (number-of-agents 3) (end-time most-positive-fixnum)
			     ((:verbose *verbose*) *verbose*))
  (when reset (reset-agent-simulation))
  (loop repeat number-of-agents do
        (schedule-event #'run-agent (list (make-instance 'agent)) 0 1))
  (schedule-event #'(lambda ()
		      (when (check-for-completion)
			(stop-simulation)))
		  nil
		  0
		  1)
  (simulate end-time))

(defun check-for-completion ()
  (every #'(lambda (agent)
	     (highest-state-p (state agent)))
	 (find-agents)))

;;;----------------------------------------------------------------------------
;;; Some utilities
  
(defun probability-p (p)
  (< (random 1.0)  p))

(defun find-agents ()
  (clip::find-instances 'agent))

(plan 3)
(format t "simple-agent-experiment~%~%")
;;; --*--
;;; ***************************************************************************

(define-simulator agent-sim-1
  :start-system (run-agent-simulation :reset nil)
  :reset-system (reset-agent-simulation)
  :stop-system stop-simulation)

;;;----------------------------------------------------------------------------
;;; Clip Definitions

(defclip agents-cost ()
  ()

  (reduce #'+ (find-agents) :key #'cost))


(defclip completion-time ()
  ()
  
  (current-time))

;;; ***************************************************************************
;;; The Experiment Definition

(define-experiment simple-agent-experiment-1 ()
  "A test experiment."
  :simulator agent-sim-1
  :ivs ((transition-probability in '(.01 .1))
	      (cost-factor from 1 to 3))
  :instrumentation (agents-cost completion-time)
  :before-trial (setf *transition-probability* transition-probability 
		      *relative-cost* cost-factor)
  :after-trial (write-current-experiment-data))

;;;----------------------------------------------------------------------------

;;(run-experiment 'simple-agent-experiment-1 :output-file "data.clasp")

(finalize)

(plan 3)
(format t "super-agent-experiment~%~%")
(finalize)

(plan 3)
(format t "agent-experiment~%~%")
(finalize)
