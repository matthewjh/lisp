(defun my-last (lst)
  (if (cdr lst)
      (my-last (cdr lst))
      (car lst)))

(defun penultimate (lst)
  (if (cddr lst)
      (penultimate (cdr lst))
      (car lst)))

(defun my-nth (lst n)
  (if (zerop n)
      (car lst)
      (my-nth (cdr lst) (1- n))))

(defun my-length (lst)
  (labels ((f (lst acc)
             (if lst
                 (f (cdr lst) (1+ acc))
                 acc)))
    (f lst 0)))

(defun my-reverse (lst)
  (labels ((f (lst acc)
             (if lst
                 (f (cdr lst) (cons (car lst) acc))
                 acc)))
    (f lst nil)))

(defun palindrome-p (lst)
  (labels ((f (lst1 lst2 acc)
             (if (and (or lst1 lst2) acc)
                 (f (cdr lst1) (cdr lst2) (equal (car lst1) (car lst2)))
                 acc)))
    (f lst (reverse lst) t)))

(defun my-flatten1 (lst)
  (let ((acc '()))
    (labels ((f (lst)
               (when lst
                 (cond
                   ((atom (car lst)) (setf acc (cons (car lst) acc))
                    (f (cdr lst)))
                   ((listp (car lst)) (f (car lst))
                    (f (cdr lst)))
                   (t (princ "Error"))))))
      (f lst)
      acc)))


(defun my-flatten2 (lst)
  (labels ((f (lst acc)
             (if lst
               (cond
                 ((atom (car lst)) (f (cdr lst) (cons (car lst) acc)))
                 ((listp (car lst)) (f (cdr lst) (f (car lst) acc))))
               acc)))
    (reverse (f lst nil))))
             

(defun compress (lst)
  (labels ((f (lst prev-car acc)
             (if lst
                 (if (and prev-car (equal (car lst) prev-car))
                     (f (cdr lst) (car lst) acc)
                     (f (cdr lst) (car lst) (cons (car lst) acc)))
                 acc)))
    (reverse (f lst nil nil))))
