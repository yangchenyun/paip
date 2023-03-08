(defpackage #:utils
    (:use #:cl)
    (:export :mappend :cross-product :combine-all :find-all :find-all-if))
    
(in-package #:utils)

(defun mappend (fn the-list)
    "Apply fn to the list and append the results"
    (apply #'append (mapcar fn the-list)))

(defun cross-product (fn xlist ylist)
    (mappend #'(lambda (y) (mapcar #'(lambda (x) (funcall fn x y)) xlist)) ylist))

(defun combine-all (xlist ylist)
  "Return a list of lists formed by appending a y to an x"
  (cross-product #'append xlist ylist))

(defun find-all (item sequence &rest keyword-args 
                 &key (test #'eql) test-not &allow-other-keys)
    (if test-not
        (apply #'remove item sequence :test-not (complement test-not) keyword-args)
        (apply #'remove item sequence :test (complement test) keyword-args)))

(setf (symbol-function 'find-all-if) #'remove-if-not)