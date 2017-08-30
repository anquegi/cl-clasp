#|
  This file is a part of extended-lisp project.
|#

#|
  Common Lisp Instrumentation Package.
|#

(in-package :cl-user)
(defpackage extended-lisp-asd
  (:use :cl :asdf))
(in-package :extended-lisp-asd)

(defsystem extended-lisp
  :version ""
  :author "Kevin Gallagher, Philip Johnson, Daniel Corkill, Kelly Murray,
David Westbrook, Marty Humphrey, Mike Greenberg, Scott Anderson
            Department of Computer and Information Science
                       University of Massachusetts
                       Amherst, Massachusetts 01003.                     "
  :maintainer "Antonio Juan Querol <antonio.querol@cuaqea.com>"
  :homepage ""
  :bug-tracker ""
  :source-control (:git "")
  :license "TODO"
  :components ((:module "src"
                :components ((:file "package")
                            (:file "lisp-extensions" :depends-on ("package")))))
  :description "Mini Version of Extended Lisp")
