(defpackage :utils
    (:export mappend cross-product combine-all))

(defun mappend (fn the-list)
    "Apply fn to the list and append the results"
    (apply #'append (mapcar fn the-list)))

(defun cross-product (fn xlist ylist)
    (mappend #'(lambda (y) (mapcar #'(lambda (x) (funcall fn x y)) xlist)) ylist))

(defun combine-all (xlist ylist)
  "Return a list of lists formed by appending a y to an x"
  (cross-product #'append xlist ylist))