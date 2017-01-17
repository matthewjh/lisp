(defun power (n p)
  (if (= p 0)
      1
      (* n (power n (1- p)))))

(defun count-atoms (lst)
  (labels ((f (curr)
             (if (atom curr)
               1
               (apply #'+ (mapcar #'f curr)))))
    (f lst)))

(defun count-atoms-alt (expr)
  (cond
    ((null expr) 0)
    ((atom expr) 1)
    (t (+ (count-atoms-alt (car expr))
          (count-atoms-alt (cdr expr))))))

(defun count-anywhere (sym expr)
  (cond
    ((atom expr) (if (eq sym expr) 1 0))
    (t (+ (count-anywhere sym (car expr))
          (count-anywhere sym (cdr expr))))))

(defun dot-product (lsta lstb)
  (apply #'+ (mapcar #'* lsta lstb))) 
