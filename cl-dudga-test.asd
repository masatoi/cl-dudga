#|
  This file is a part of cl-dudga project.
|#

(in-package :cl-user)
(defpackage cl-dudga-test-asd
  (:use :cl :asdf))
(in-package :cl-dudga-test-asd)

(defsystem cl-dudga-test
  :author ""
  :license ""
  :depends-on (:cl-dudga
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "cl-dudga"))))
  :description "Test system for cl-dudga"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
