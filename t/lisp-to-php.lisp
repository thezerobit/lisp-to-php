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

(plan 12)

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
(is (ltp "(let ((x
                  (let ((y 10)) (var_dump y) (+ y 20))))
            (var_dump (+ x 90)))")
"$x_ = call_user_func(function  () {
    $y_ = 10;
    var_dump($y_);
    return ($y_ + 20);
});
var_dump(($x_ + 90));
"   "LET as expression (requiring anon function).")
(is (ltp "(let ((x
                  (let ((y 10)) (var_dump y) (+ y z))))
            (var_dump (+ x 90)))")
"$x_ = call_user_func(function  () use ($z) {
    $y_ = 10;
    var_dump($y_);
    return ($y_ + $z);
});
var_dump(($x_ + 90));
"   "LET as expression (requiring anon function w/capture).")

(is (ltp "(defun foo (x) (var_dump x) (+ x 10))")
"function foo ($x) {
    var_dump($x);
    return ($x + 10);
}" "Normal function definition.")

(is (ltp "(let ((y 400)) (defun foo (x) (var_dump x) (+ x y)))")
"$y_ = 400;
function foo ($x) {
    var_dump($x);
    return ($x + $y);
}" "Normal function definition, variable capture not allowed.")

(is (ltp "(defun foo (x)
            (var_dump x)
            (let ((y 400))
              (+ x y)))")
"function foo ($x) {
    var_dump($x);
    return call_user_func(function  () use ($x) {
        $y_ = 400;
        return ($x + $y_);
    });
}" "Normal function definition with inner LET.")

(finalize)
