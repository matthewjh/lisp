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
