;;; Version 2.1, June 1992.
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
;;; -- * --

(in-package "COMMON-LISP-USER")

(cl:defpackage "RTM"
  (:use "COMMON-LISP"))

(use-package "RTM")

;;; -- * --
;; This file contains rtm test functions based on an example database application.

(defun test (&optional (warn t) (notify t))
  (setf *rtm-warning-stream* warn)
  (setf *rtm-notification-stream* notify)
  (rtm-reset)
  (test-create)
  (test-insert)
  (test-update)
  (test-delete)
  ;;(test-save)
  (test-sort)
  (test-indexes)
  (test-where)
  (test-join)
  (test-neighbors)
  (test-views)
  (test-distinct)
  (values t))

(defun test-compare (value-1 value-2)
  (unless (equal value-1 value-2)
    (format t "~%RTM test failure: ~s vs. ~s" value-1 value-2)
    (return-from test-compare (values nil)))
  (values t))

(defun test-create ()
  
  (create-table
    'department
    :attributes '((name :string :key :index)
                  (chair :integer)
                  (building :string)
                  (room :integer)
                  (phone :string))
    :initial-size 20)
  
  (test-compare t (rtm-table-p 'department))
  (test-compare '(name chair building room phone)
                (table-attributes 'department))
  (test-compare 0 (count-selection :from 'department))
  
  (create-table
    'faculty
    :attributes '((id :integer :key :index)
                  (last-name :string :index)
                  (first-name :string)
                  (title :string)
                  (dept :string)
                  (building :string)
                  (room :integer)
                  (phone :string)))
  
  (test-compare t (rtm-table-p 'faculty))
  (test-compare '(id last-name first-name title dept building room phone)
                (table-attributes 'faculty))
  (test-compare 0 (count-selection :from 'faculty))
  
  (create-table
    'student
    :attributes '((id :integer :key :index)
                      (last-name :string :index)
                      (first-name :string)
                      (enrollment-date :date :index)
                      (major-dept :string)
                      (advisor :integer)))
  
  (test-compare t (rtm-table-p 'student))
  (test-compare '(id last-name first-name enrollment-date major-dept advisor)
                (table-attributes 'student))
  (test-compare 0 (count-selection :from 'student))
  
  (create-table
    'temp
    :attributes '((x :uppercase-string :index)
                  (y :lowercase-string :key)
                  (z :single-float :key :index)
                  (w :fixnum :index :key)))
  
  (test-compare t (rtm-table-p 'temp))
  (drop-table 'temp)
  (test-compare nil (rtm-table-p 'temp)))

(defun test-insert ()
  
  (insert-into-table
   'department
   '(name "COINS" chair 1000
     building "LGRC" room 305 phone "545-2744"))
  (insert-into-table
   'department
   '(name "PSYCH" chair 2000
     building "Tobin" room 401 phone "545-2383"))
  (insert-into-table
   'department
   '(name "IE&OR" chair 3000
     building "Marston" room 114 phone "545-2851"))
  (insert-into-table
   'department
   '(name "PHIL" chair 4000
     building "Bartlett" room 352 phone "545-2330"))
  
  (test-compare 4 (count-selection :from 'department))
  
  
  (insert-into-table
   'faculty
   '(id 1000 last-name "Adrion" first-name "Rick" title "Chairperson"
     dept "COINS" building "LGRC" room 307 phone "545-2742"))
  
  (insert-into-table
   'faculty
   '(id 2000 title "Chairperson" dept "PSYCH" building "Tobin"))
  
  (insert-into-table
   'faculty
   '(id 3000 title "Chairperson" dept "IE&OR" building "Marston"))
  
  (insert-into-table
   'faculty
   '(id 4000 title "Chairperson" dept "PHIL" building "Bartlett"))
  
  (insert-into-table
   'faculty
   '(id 1001 last-name "Cohen" first-name "Paul" title "Associate Professor"
     dept "COINS" building "LGRC" room 335 phone "545-3638"))
  
  (insert-into-table
   'faculty
   '(id 1002 last-name "Utgoff" first-name "Paul" title "Assistant Professor"
     dept "COINS" building "LGRC" room 341 phone "545-4843"))
  
  (test-compare 6 (count-selection :from 'faculty))
  
  (insert-into-table
   'faculty
   '(id 1003 last-name "Rosenberg" first-name "Arnold" title "Professor"
     dept "COINS" building "LGRC" room 267 phone "545-2743"))
  
  (insert-into-table
   'faculty
   '(id 1004 last-name "Moss" first-name "Eliot" title "Assistant Professor"
     dept "COINS" building "LGRC" room 351 phone "545-4206"))
  
  (insert-into-table
   'faculty
   '(id 1005 last-name "Coury" first-name "Bruce" title "Assistant Professor"
     dept "IE&OR" building "Marston" phone "545-2854"))
  
  (insert-into-table
   'faculty
   '(id 1006 last-name "Meyers" first-name "Jerry" title "Professor"
     dept "PSYCH" building "Tobin" phone "545-2331"))
  
  (test-compare 10 (count-selection :from 'faculty))
  
  
  (insert-into-table
   'student
   '(id 10001 last-name "Silvey" first-name "Monica"
     enrollment-date "9/1/88"
     major-dept "COINS" advisor 1001))
  
  (insert-into-table
   'student
   '(id 10004 last-name "Howe" first-name "Adele"
     enrollment-date "9/1/83"
     major-dept "COINS" advisor 1001))
  
  (insert-into-table
   'student
   '(id 10006 last-name "Smith" first-name "Evan"
     enrollment-date "9/1/89"
     major-dept "COINS" advisor 1001))
  
  (insert-into-table
   'student
   '(id 10005 last-name "Mammen" first-name "Dorthy"
     enrollment-date "9/1/88"
     major-dept "COINS" advisor 1001))
  
  (insert-into-table
   'student
   '(id 10002 last-name "Anderson" first-name "Scott"
     enrollment-date "9/1/84"
     major-dept "COINS" advisor 1001))
  
  (insert-into-table
   'student
   '(id 10003 last-name "Brown" first-name "Eric"
     enrollment-date "9/1/89"
     major-dept "COINS" advisor 1004))
  
  (insert-into-table
   'student
   '(id 10010 last-name "Hoskings" first-name "Tony"
     enrollment-date "9/1/86"
     major-dept "COINS" advisor 1004))
  
  (insert-into-table
   'student
   '(id 10007 last-name "Brodley" first-name "Carla"
     enrollment-date "9/1/88"
     major-dept "COINS" advisor 1001))
  
  (insert-into-table
   'student
   '(id 10009 last-name "Fawcett" first-name "Tom"
     enrollment-date "9/1/88"
     major-dept "COINS" advisor 1002))
  
  (insert-into-table
   'student
   '(id 10008 last-name "Loiselle" first-name "Cindy"
     enrollment-date "9/1/82"
     major-dept "COINS" advisor 1001))
  
  (insert-into-table
   'student
   '(id 10011 last-name "Fisher" first-name "David"
     enrollment-date "9/1/89"
     major-dept "COINS" advisor 1001))
  
  (test-compare 11 (count-selection :from 'student)))

(defun test-views ()

  (when (rtm-table-p 'total-set)
    (drop-table 'total-set))

  (create-table
    'total-set
    :attributes '((num1 :integer :key :index)
                  (num2 :integer :index))
    :initial-size 2000)
  (dotimes (i 2000)
    (insert-into-table 'total-set `(num1 ,i num2 ,(random 1000))))
  (test-compare 2000 (count-selection :from 'total-set))
  
  (create-view 'partial-set :from 'total-set :where (.<<. 'num1 400))
  
  (create-view 'p2 :from 'partial-set :where (.>=. 'num1 200))
  
  (test-compare 50 (count-selection :from 'p2 :where (.<<. 'num1 250)))
  
  (test-compare 100 (count-selection :from 'partial-set :where (.=><=. 'num1 300 600)))
  
  (drop-view 'partial-set)
  (drop-view 'p2)
  (drop-table 'total-set)
  
  )

(defun test-update ()
  
  (test-compare 8 (count-selection :from 'student :where (.==. 'advisor 1001)))
  (test-compare 1 (count-selection :from 'student :where (.==. 'advisor 1002)))
  (do-selection (:from 'student :where (.==. 'advisor 1001))
    (when (string= "Brodley" (attr-value 'last-name))
      (setf (attr-value 'advisor) 1002)))
  (test-compare 7 (count-selection :from 'student :where (.==. 'advisor 1001)))
  (test-compare 2 (count-selection :from 'student :where (.==. 'advisor 1002)))
  
  (create-index 'student 'first-name)
  (test-compare 1 (count-selection :from 'student :where (.==. 'first-name "Monica")))
  (do-selection (:from 'student :where (.==. 'first-name "Monica"))
    (setf (attr-value 'first-name) "Paul"))
  (test-compare 0 (count-selection :from 'student :where (.==. 'first-name "Monica")))
  (test-compare 1 (count-selection :from 'student :where (.==. 'first-name "Paul"))))

(defun test-save ()
  ;; Load / Store test:
  (store-table 'department)
  (drop-table 'department)
  (test-compare nil (rtm-table-p 'department))
  (load-table 'department)
  (test-compare t (rtm-table-p 'department))
  (test-compare '(name chair building room phone)
                (table-attributes 'department))
  (test-compare 4 (count-selection :from 'department)))

(defun test-sort ()
  (test-compare "COINS" (min-selection :attr 'name :from 'department))
  (test-compare "PSYCH" (max-selection :attr 'name :from 'department))
  
  (test-compare "9/1/82" (min-selection :attr 'enrollment-date :from 'student))
  (test-compare "9/1/89" (max-selection :attr 'enrollment-date :from 'student))
  
  (list-selection :attrs '(first-name last-name)
                  :from 'student
                  :order-by '(enrollment-date :desc last-name)))

(defun test-indexes ()
  (test-compare t (index-p 'department 'name))
  (drop-index 'department 'name)
  (test-compare nil (index-p 'department 'name))
  (test-where)
  (test-compare nil (index-p 'department 'name))
  (create-index 'department 'name)
  (test-compare t (index-p 'department 'name))
  (test-where))

(defun test-where ()
  (let ((min (min-selection :attr 'name :from 'department))
        (max (max-selection :attr 'name :from 'department)))
    
    (test-compare 1 (count-selection :from 'department :where (.==. 'name "COINS")))
    (test-compare 3 (count-selection :from 'department :where (./=. 'name "PHIL")))
    
    (test-compare 0 (count-selection :from 'department
                                     :where (.<<>>. 'name min max)))
    (test-compare 1 (count-selection :from 'department
                                     :where (.<=>>. 'name min max)))
    (test-compare 1 (count-selection :from 'department
                                     :where (.<<=>. 'name min max)))
    (test-compare 2 (count-selection :from 'department
                                     :where (.>><<. 'name min max)))
    (test-compare 2 (count-selection :from 'department
                                     :where (.<==>. 'name min max)))
    (test-compare 3 (count-selection :from 'department
                                     :where (.=><<. 'name max min)))
    (test-compare 3 (count-selection :from 'department
                                     :where (.>><=. 'name max min)))
    (test-compare 4 (count-selection :from 'department
                                     :where (.=><=. 'name max min)))
    
    (test-compare (min-selection :attr 'name :from 'department)
                  (first (list-selection :attrs 'name :from 'department
                                         :where (.min. 'name))))
    (test-compare (max-selection :attr 'name :from 'department)
                  (first (list-selection :attrs 'name :from 'department
                                         :where (.max. 'name))))
    
    (test-compare "IE&OR" (first (list-selection :attrs 'name
                                                 :from 'department
                                                 :where (.succ. 'name "COINS"))))
    (test-compare "COINS" (first (list-selection :attrs 'name
                                                 :from 'department
                                                 :where (.ceiling. 'name "COINS"))))
    (test-compare "PHIL" (first (list-selection :attrs 'name
                                                :from 'department
                                                :where (.pred. 'name "PSYCH"))))
    (test-compare "PSYCH" (first (list-selection :attrs 'name
                                                 :from 'department
                                                 :where (.floor. 'name "PSYCH"))))
    
    (test-compare nil (first (list-selection :attrs 'name
                                             :from 'department
                                             :where (.pred. 'name "COINS"))))
    (test-compare nil (first (list-selection :attrs 'name
                                             :from 'department
                                             :where (.floor. 'name "A"))))
    
    (test-compare nil (first (list-selection :attrs 'name
                                             :from 'department
                                             :where (.succ. 'name "PSYCH"))))
    (test-compare nil (first (list-selection :attrs 'name
                                             :from 'department
                                             :where (.ceiling. 'name "Z"))))))

(defun test-join ()
  (test-compare t (row-exists-p :in-table 'faculty :where (.==. 'first-name "Paul")))
  (let ((outer 0)
        (inner 0))
    (do-selection (:from 'faculty :where (.==. 'first-name "Paul"))
      (let ((id (attr-value 'id)))
        ;;(format t "~%~%~a ~a" (attr-value 'first-name) (attr-value 'last-name))
        (incf outer)
        (do-selection (:from 'student
                             :where (.==. 'advisor id)
                             :order-by '(enrollment-date last-name))
          ;;(format t "~%   ~a ~a" (attr-value 'first-name) (attr-value 'last-name))
          (incf inner))))
    (test-compare 2 outer)
    (test-compare 9 inner)))

(defun test-delete ()
  (let ((n (count-selection :from 'faculty))
        (k (count-selection :from 'faculty :where (.==. 'first-name ""))))
    (delete-selection :from 'faculty :where (.==. 'first-name ""))
    (test-compare (- n k) (count-selection :from 'faculty))
    (test-compare 0 (count-selection :from 'faculty :where (.==. 'first-name "")))))

(defun test-neighbors ()
  (let ((q-val "9/1/87"))
    (unless (index-p 'student 'enrollment-date)
      (create-index 'student 'enrollment-date))
    (do-neighbors (dist :from-point `(enrollment-date ,q-val)
                        :in-table 'student
                        :distance-metric :city-block)
      (format t "~%~a, ~15tenrolled ~a, ~5d ~38tdays from ~a"
              (attr-value 'last-name)
              (attr-value 'enrollment-date) (round dist) q-val))))

(defun test-distinct ()
  
  (create-table
    'xyzzy
    :attributes '((x :integer :index)
                  (y :fixnum)
                  (z :string :index)
                  (a :date :index)
                  (b :string)
                  (c :integer)))
  
  (insert-into-table 'xyzzy '(x 1 y 1 z "xyz" a "7/17/60" b "test" c 4))
  (insert-into-table 'xyzzy '(x 1 y 1 z "abc" a "7/17/60" b "test" c 1))
  (insert-into-table 'xyzzy '(x 1 y 2 z "abc" a "1/29/60" b "test" c 2))
  (insert-into-table 'xyzzy '(x 1 y 1 z "xyz" a "7/17/60" b "test" c 3))
  (insert-into-table 'xyzzy '(x 1 y 2 z "abc" a "7/17/60" b "test" c 1))
  (insert-into-table 'xyzzy '(x 1 y 1 z "abc" a "1/29/60" b "test" c 2))
  (insert-into-table 'xyzzy '(x 1 y 1 z "xyz" a "7/17/60" b "test" c 3))
  (insert-into-table 'xyzzy '(x 1 y 1 z "xyz" a "7/17/60" b "test" c 2))
  
  (test-compare 2 (count-selection :distinct 'y :from 'xyzzy))
  (test-compare 3 (count-selection :distinct '(y z) :from 'xyzzy))
  (test-compare 2 (count-selection :distinct '(x b a) :from 'xyzzy))
  (test-compare 5 (count-selection :distinct '(z c) :from 'xyzzy))
  )

;;;
;;;  Delete bug in list domain with index.  Can't duplicate yet.
;;; 

(defun rtm-bug? ()
  
  (create-domain
   'list
   :less-p #'(lambda (val1 val2)
               (cond
                ((null val1) t)
                ((null val2) nil)
                (t (< (length  val1) (length  val2)))))
   :equal-p #'(lambda (val1 val2)
                (not (member nil (mapcar #'equal val1 val2))))
   :encode-fn #'(lambda (val)
                  (cond
                   ((null val) val)
                   ((listp val) val)
                   (t :error)))
   :verify-fn #'(lambda (val) (or (typep val 'list) (null val)))
   :default-value nil)
  
  (create-domain
   'float-with-nil
   :less-p #'(lambda (val1 val2)
               (cond
                ((null val1) t)
                ((null val2) nil)
                (t (< val1 val2))))
   :equal-p #'equal
   :encode-fn #'(lambda (val)
                  (cond
                   ((null val) val)
                   ((and (numberp val)
                         (not (typep val 'complex)))
                    (coerce val 'single-float))
                   (t :error)))
   :verify-fn #'(lambda (val)
                  (or (typep val 'single-float) (null val)))
   :default-value nil)
  
  (load-table 'table)
  ;;(store-table 'table)
  
  (count-selection :from 'table)
  
  (list-selection :attrs 'var-24 :from 'table :where (.==. 'var-10 nil))
  (count-selection :from 'table :where (.==. 'var-10 nil))
  
  (do-selection (:from 'table :where (.==. 'var-10 nil))
    (format t "~%~s ~s" (attr-value 'var-10) (attr-value 'var-24)))
  
  ;;(create-index 'table 'var-24)
  ;;(create-index 'table 'var-10)
  
  ;;(index-p 'table 'var-24)
  
  (delete-selection :from 'table :where (.==. 'var-10 nil))
  
  ;;(drop-index 'table 'var-10)
  (drop-table 'table)
  )

