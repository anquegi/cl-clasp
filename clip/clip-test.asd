#|
  This file is a part of ningle project.
  Copyright (c) 2012-2014 Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage clip-test-asd
  (:use :cl :asdf))
(in-package :clip-test-asd)

(defsystem clip-test
  :author "Antonio Juan Querol Giner"
  :license "MIT"
  :description "Tests for CLIP, translate demos into tests"
  :depends-on (:clip
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "clip"))))
  :defsystem-depends-on (:prove)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove) c )))
