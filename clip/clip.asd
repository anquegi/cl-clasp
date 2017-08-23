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
  :author ""
  :license "MIT"
  :components ((:module "src"
                :components
                ((:file "" :depends-on (""))
                 (:file "" :depends-on ("" ""))
                 (:file "")
                 (:file ""))))
  :description "Common Lisp Instrumentation Package."
:in-order-to ((test-op (test-op ningle-test))))
