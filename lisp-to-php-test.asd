#|
  This file is a part of lisp-to-php project.
  Copyright (c) 2012 Stephen A. Goss (steveth45@gmail.com)
|#

(in-package :cl-user)
(defpackage lisp-to-php-test-asd
  (:use :cl :asdf))
(in-package :lisp-to-php-test-asd)

(defsystem lisp-to-php-test
  :author "Stephen A. Goss"
  :license "Modified BSD License"
  :depends-on (:lisp-to-php
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:file "lisp-to-php"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
