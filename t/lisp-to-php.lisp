#|
  This file is a part of lisp-to-php project.
  Copyright (c) 2012 Stephen A. Goss (steveth45@gmail.com)
|#

(in-package :cl-user)
(defpackage lisp-to-php-test
  (:use :cl
        :lisp-to-php
        :cl-test-more))
(in-package :lisp-to-php-test)

(plan 7)

(is (ltp "\"Steve's\"") "'Steve\\'s'" "PHP Strings.")
(is (ltp "(let ((a b)) (setf a \"hello\") (print a))")
"$a_ = $b;
$a_ = 'hello';
print($a_);
" "Basic LET works.")
(is (ltp ":foo") "'foo'" "Keywords are strings.")
(is (ltp "(fizz buzz \"wow\")")
"fizz($buzz, 'wow');
" "Function calls.")
(is (ltp "(? / !)") "what($slash, $bang);
" "PHP friendly symbol conversion.")
(is (ltp "(let ((a 10))
            (if (> a 5)
              (var_dump \"hello\")
              (var_dump \"goodbye\")))")
"$a_ = 10;
if(greaterthan($a_, 5)) {
    var_dump('hello');
} else {
    var_dump('goodbye');
}
"   "LET and IF.")
(is (ltp "10/100") "0.1" "Number support.")

(finalize)
