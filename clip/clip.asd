#|
  This file is a part of clip project.
|#

#|
  Common Lisp Instrumentation Package.
|#

(in-package :cl-user)
(defpackage clip-asd
  (:use :cl :asdf))
(in-package :clip-asd)

(defsystem clip
  :version "1.4.0"
  :author "David L. Westbrook
Experimental Knowledge Systems Laboratory
Paul R. Cohen, Principal Investigator
David L. Westbrook, Systems Manager
David M. Hart, Laboratory Manager
Department of Computer Science
University of Massachusetts0
Amherst, Massachusetts 01003."
  :maintainer "Antonio Juan Querol <antonio.querol@cuaqea.com>"
  :homepage ""
  :bug-tracker ""
  :source-control (:git "")
  :license "TODO"
  :depends-on (:lambda.time
               :extended-lisp)
  :components ((:module "src"
                :components ((:file "packages")
                (:file "utilities" :depends-on ("packages"))
                (:file "super-intrinsic-mixins" :depends-on ("packages"))
                (:file "time-definitions" :depends-on ("packages"))
                (:file "macros" :depends-on ("packages"))
                (:file "parameters" :depends-on ("packages"))
                (:file "intrinsic-mixins" :depends-on ("packages"))
                (:file "class-defs" :depends-on ("packages"))
                (:file "instrumentation" :depends-on ("packages"))
                (:file "defclip" :depends-on ("packages"))
                (:file "simulator" :depends-on ("packages"))
                (:file "experiment-runner" :depends-on ("packages"))
                (:file "standard-clips" :depends-on ("packages"))
                (:file "define-experiment" :depends-on ("packages")))))
  :description "CLIP Common Lisp Instrumentation Package."
  :in-order-to ((test-op (test-op clip-test))))
