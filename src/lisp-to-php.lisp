#|
  This file is a part of lisp-to-php project.
  Copyright (c) 2012 Stephen A. Goss (steveth45@gmail.com)
|#

(in-package :cl-user)
(defpackage lisp-to-php
  (:use :cl :cl-ppcre)
  (:export :ltp))
(in-package :lisp-to-php)

;;TODO:
;; lambda
;; progn
;; defun -> function
;; infix operators (+, -, *, /)
;; method calls (->bar foo 1 2 3) -> $foo->bar(1, 2, 3)
;; number literals
;; strings "What's up?" -> 'What\'s up?'
;; variable name fixes this-is-it! -> thisIsItbang
;; funcall
;; apply
;; macros?
;; setq / setf
;; array support / gethash setf-able

(defparameter *cs-readtable* (let ((csrt (copy-readtable nil)))
                               (setf (readtable-case csrt) :preserve)
                               csrt))

(defparameter *infix-operators* '(+ - * /))

(defun ci-read-from-string (string &rest args)
  (let ((*readtable* *cs-readtable*))
    (apply #'read-from-string string args)))

(defun make-env () '())

(defun env-add-varnames (env varnames)
  (concatenate 'list varnames env))

(defun get-name (symbol env)
  (let ((name (string symbol)))
    (dotimes (x (count symbol env))
      (setq name (concatenate 'string name "_")))
    name))

(defun get-free-variables (form &optional (ignore '()))
  (let ((varnames '()))
    (if (listp form)
      (let ((head (car form)))
        (cond
          ((null head) nil)
          ((eq '|let| head)
           (let* ((more-to-ignore (mapcar #'car (cadr form)))
                  (new-ignore (union ignore more-to-ignore)))
             (dolist (binding (cadr form))
               (setq varnames (union varnames (get-free-variables (cadr binding) ignore))))
             (dolist (expr (cddr form))
               (setq varnames (union varnames (get-free-variables expr new-ignore))))))
          (T (dolist (subform (cdr form))
               (setq varnames (union varnames (get-free-variables subform ignore)))))))
      (if (and
            (symbolp form)
            (not (keywordp form))
            (not (member form ignore)))
        (push form varnames)))
    varnames))

(defun val (key alist)
  (cdr (assoc key alist)))

(defun add-indent (string &optional (doit T))
  (if doit
    (regex-replace-all "(.+)" string "    \\1")
    string))

(defun form-to-php (form env &optional (top-level nil) (stmt nil))
  (if (listp form)
    (let ((head (car form))
          (tail (cdr form) ))
      (cond
        ((eq '|if| head)
         (if-to-php tail env top-level stmt))
        ((eq '|let| head)
         (let-to-php form env top-level stmt))
        ;; TODO: handle lambda
        (T (format nil "~a(~{~a~^, ~})~[;~%~]" head
                   (mapcar (lambda (x) (form-to-php x env nil nil)) tail)
                   (if stmt 0 1)))))
    (concatenate 'string "$"
                 (get-name form env)
                 (if stmt (format nil ";~%") ""))))

(defun if-to-php (tail env &optional (top-level nil) (stmt nil))
  (let* ((length-tail (length tail))
         (error (when (or (< length-tail 2) (> length-tail 3))
                  (error "Invalid number of args to 'if: ~a." length-tail)))
         (base-pattern
           (cond
             (stmt
              (if (= 2 length-tail)
                "if(~a) {~%~a}~%"
                "if(~a) {~%~a} else {~%~a}~%"))
             (T
              (if (= 2 length-tail)
                "((~a) ? (~a) : null)"
                "((~a) ? (~a) : (~a))")))))
    (if (= 1 length-tail)
      (format nil base-pattern
              (form-to-php (car tail) env)
              (add-indent
                (form-to-php (cadr tail) env nil stmt)
                stmt))
      (format nil base-pattern
              (form-to-php (car tail) env)
              (add-indent
                (form-to-php (cadr tail) env nil stmt)
                stmt)
              (add-indent
                (form-to-php (caddr tail) env nil stmt)
                stmt)))))

(defun binding-to-php (symbol expr env new-env &optional (top-level nil) (stmt nil))
  (when (not (symbolp symbol))
    (error "Left side of binding must be symbol: ~a" symbol))
  (format nil "~a = ~a;~%"
          (form-to-php symbol new-env top-level)
          (form-to-php expr env top-level)))

(defun let-to-php (form env &optional (top-level nil) (stmt nil))
  (let* ((tail (cdr form))
         (bindings (car tail))
         (varnames (mapcar #'car (car tail)))
         (forms (cdr tail))
         (new-env (env-add-varnames env varnames))
         (php-bindings (mapcar (lambda (binding)
                                 (binding-to-php (car binding) (cadr binding)
                                                 env new-env top-level))
                               bindings)))
    (if stmt
      (format nil "~{~a~}~{~a~}"
              php-bindings
              (mapcar (lambda (form)
                        (form-to-php form new-env top-level T))
                      forms))
      (let* ((free-vars (get-free-variables form))
             (format-string
               (if free-vars
                 "call_user_func(function () use (~{~a~^, ~}) {~%~{~a~}~{~a~}~a})"
                 "call_user_func(function () ~{ ~} {~%~{~a~}~{~a~}~a})")))
        (format nil format-string
                (mapcar (lambda (x) (form-to-php x env)) free-vars)
                (mapcar #'add-indent php-bindings)
                (mapcar (lambda (x) (add-indent (form-to-php x new-env nil T))) (butlast forms))
                (add-indent (format nil "return ~a;~%" (form-to-php (car (last forms)) new-env))))))))

(defun ltp (string)
  (let ((more T)
        (position 0)
        (php-strings '()))
     (loop while more
           do (multiple-value-bind (val pos)
                  (ci-read-from-string string nil 'eof :start position)
                (if (eq val 'eof)
                  (setq more nil)
                  (push (form-to-php val (make-env) T T) php-strings))
                (setq position pos)))
     (format nil "~{~a~}" (nreverse php-strings))))


;; (ltp "(let ((x (let ((b y)) (print b) (foo bar b)))) (+ x y))")
