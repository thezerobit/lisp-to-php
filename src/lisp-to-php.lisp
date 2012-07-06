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
;; optional args to defun
;; lambda
;; progn
;; array support (vector), literals: #(1 2 3)
;; method calls (->bar foo 1 2 3) -> $foo->bar(1, 2, 3)
;; number literals
;; funcall
;; apply
;; macros?
;; array support / gethash setf-able

(defparameter *cs-readtable* (let ((csrt (copy-readtable nil)))
                               (setf (readtable-case csrt) :preserve)
                               csrt))

(defun ci-read-from-string (string &rest args)
  (let ((*readtable* *cs-readtable*)
        (*package* (find-package 'lisp-to-php)))
    (apply #'read-from-string string args)))

(defparameter *infix* (ci-read-from-string "(+ - * /)"))
;; TODO these php operators: % . >> << & ^ | && || and xor or
(defparameter *numeric-comparison* (ci-read-from-string "(= /= < > <= >=)"))
(defparameter *unary* (ci-read-from-string "(clone new ++ -- ~ - !)"))
(defparameter *replacements* '(("=" . "equals")
                               ("/" . "slash")
                               ("<" . "lessthan")
                               (">" . "greaterthan")
                               ("!" . "bang")
                               ("#" . "hash")
                               ("\\?" . "what")
                               ("-" . "_")))

(defun make-env () '())

(defun env-add-varnames (env varnames)
  (concatenate 'list varnames env))

(defun get-name (symbol env)
  (let ((name (php-name symbol)))
    (dotimes (x (count symbol env))
      (setq name (concatenate 'string name "_")))
    name))

(defun var-names-from-arglist (arglist)
  (let ((var-names '()))
    (dolist (item arglist)
      (when (and (symbolp item) (not (keywordp item)))
        (push item var-names)))
    var-names))

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
          ((eq '|defun| head)
           (let* ((more-to-ignore (var-names-from-arglist (caddr form)))
                  (new-ignore (union ignore more-to-ignore)))
             (dolist (expr (cdddr form))
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

(defun php-name (name)
  (let ((name (string name)))
    (dolist (map *replacements*)
      (setq name (regex-replace-all (car map) name (cdr map))))
    name))

(defun form-to-php (form env &optional (top-level nil) (stmt nil))
  (if (listp form)
    (let ((head (car form))
          (tail (cdr form) ))
      (cond
        ((eq '|if| head)
         (if-to-php tail env top-level stmt))
        ((eq '|let| head)
         (let-to-php form env top-level stmt))
        ((eq '|defun| head)
         (defun-to-php form env top-level stmt))
        ((member head '(|setq| |setf|))
         (setf-to-php form env top-level stmt))
        ((member head *infix*)
         (infix-to-php head tail env top-level stmt))
        ((member head *unary*)
         (unary-to-php head tail env top-level stmt))
        ;; TODO: handle lambda
        (T (format nil "~a(~{~a~^, ~})~[;~%~]" (php-name head)
                   (mapcar (lambda (x) (form-to-php x env nil nil)) tail)
                   (if stmt 0 1)))))
    (cond
      ((numberp form) (number-to-php form))
      ((stringp form) (string-to-php form))
      ((keywordp form) (string-to-php (php-name form)))
      ((vectorp form) (format nil "array(~{~a~^, ~})~[;~%~]"
                   (mapcar (lambda (x) (form-to-php x env nil nil))
                           (concatenate 'list form))
                   (if stmt 0 1)))
      (T (concatenate 'string "$"
                      (get-name form env)
                      (if stmt (format nil ";~%") ""))))))

(defun number-to-php (number)
  (cond
    ((integerp number) (format nil "~a" number))
    (T (format nil "~a" (float number)))))

(defun string-to-php (form)
  (format nil "'~a'" (regex-replace-all "\\'" form "\\\\'")))

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

(defun lambda-to-php (forms &key (name "") args free-vars php-bindings outer-env inner-env)
  (let ((format-string
          (concatenate 'string
                       (if name "function ~a " "function ~a")
                       "(~{~a~^, ~}) "
                       (if free-vars "use (~{~a~^, ~}) " "~{ ~}")
                       "{~%~{~a~}~{~a~}~a}")))
    (format nil format-string
            name
            (mapcar (lambda (x) (form-to-php x inner-env)) args)
            (mapcar (lambda (x) (form-to-php x outer-env)) free-vars)
            (mapcar #'add-indent php-bindings)
            (mapcar (lambda (x) (add-indent (form-to-php x inner-env nil T))) (butlast forms))
            (add-indent (format nil "return ~a;~%" (form-to-php (car (last forms)) inner-env))))))

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
      (let* ((free-vars (get-free-variables form)))
        (format nil "call_user_func(~a)"
                (lambda-to-php forms
                               :free-vars free-vars
                               :php-bindings php-bindings
                               :outer-env env
                               :inner-env new-env))))))

(defun defun-to-php (form env &optional (top-level nil) (stmt nil))
  (let* ((name (cadr form))
         (arglist (caddr form))
         (body (cdddr form))
         (free-vars (get-free-variables form))
         (varnames (var-names-from-arglist arglist)))
    (lambda-to-php body
                   :name (php-name name)
                   :args varnames)))

(defun setf-to-php (form env &optional (top-level nil) (stmt nil))
  (if stmt
    (format nil "~a = ~a;~%" (form-to-php (cadr form) env top-level nil)
            (form-to-php (caddr form) env top-level nil))
    (format nil "(~a = ~a)" (form-to-php (cadr form) env top-level nil)
            (form-to-php (caddr form) env top-level nil))))

(defun infix-to-php (op tail env &optional (top-level nil) (stmt nil))
  (when (< (length tail) 2)
    (error "infix operator '~a' requires at least 2 arguments" op))
  (format nil (format nil "(~~{~~a~~^ ~a ~~})" op)
          (mapcar (lambda (x) (form-to-php x env top-level nil))
                  tail)))

(defun unary-to-php (op tail env &optional (top-level nil) (stmt nil))
  (when (not (= (length tail) 1))
    (error "unary operator '~a' requires exactly 1 argument" op))
  (format nil "(~a ~a)" op (form-to-php (car tail) env top-level nil)))

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
