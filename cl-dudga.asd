;;; -*- coding:utf-8; mode:lisp -*-

(in-package :cl-user)
(defpackage cl-dudga-asd
  (:use :cl :asdf))
(in-package :cl-dudga-asd)

(defsystem cl-dudga
  :version "0.1"
  :author "Satoshi Imai"
  :license "MIT License"
  :depends-on (:lparallel)
  :components ((:module "src"
                :components
                ((:file "cl-dudga"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.org"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op cl-dudga-test))))
