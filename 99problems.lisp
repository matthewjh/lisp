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

(defun pack (lst)
  (labels ((f (lst prev-car acc bundle)
             (if lst
                 (if (and prev-car (equal (car lst) prev-car))
                     (f (cdr lst) (car lst) acc (cons (car lst) bundle))
                     (f (cdr lst)
                        (car lst)
                        (if bundle (cons bundle acc) acc)
                        (list (car lst))))
                 (if bundle (cons bundle acc) acc))))
    (reverse (f lst nil nil nil))))

(defun encode (lst)
  (let ((packed (pack lst)))
    (mapcar (lambda (l)
              (list (length l) (car l)))
            packed)))

(defun encode-modified (lst)
  (let ((packed (pack lst)))
    (mapcar (lambda (l)
              (if (> (length l) 1)
                  (list (length l) (car l))
                  (car l)))
            packed)))

(defun decode (lst)
  (labels ((repeat (i n acc)
             (if (> n 0)
                 (repeat i (1- n) (cons i acc))
                 acc)))
    (mapcan (lambda (el)
              (let ((lst (if (atom el) (list 1 el) el))) 
                (repeat (cadr lst) (car lst) nil)))
            lst)))

(defun encode-direct (lst)
  (labels ((f (lst prev-car count acc)
             (if lst
                 (if (and prev-car (equal prev-car (car lst)))
                     (f (cdr lst) prev-car (1+ count) acc)
                     (if prev-car
                         (f (cdr lst) (car lst) 1 (cons (list count prev-car) acc))
                         (f (cdr lst) (car lst) 1 acc)))
                      
                 (cons (list count prev-car) acc))))
    (mapcar (lambda (lst)
              (if (equal (car lst) 1)
                  (cadr lst)
                  lst))
            (reverse (f lst nil nil nil)))))

(defun dupli (lst)
  (mapcan (lambda (i)
            (list i i))
          lst))

(defun repli (lst n)
    (labels ((repeat (i n acc)
             (if (> n 0)
                 (repeat i (1- n) (cons i acc))
                 acc)))
    (mapcan (lambda (i)
              (repeat i n nil))
            lst)))

(defun drop (lst original-n)
  (labels ((f (lst n acc)
             (if lst
                 (if (= n 0)
                     (f (cdr lst) original-n acc)
                     (f (cdr lst) (1- n) (cons (car lst) acc)))
                 acc)))
    (reverse (f lst original-n nil))))
           
(defun split (lst n)
  (labels ((f (lst n acc)
             (if (and lst (> n 0))
                 (f (cdr lst) (1- n) (cons (car lst) acc))
                 (values (reverse acc) lst))))
    (f lst n nil)))

(defun slice (lst s e)
  (labels ((f (lst n acc)
              (if (and lst (<= n e))
                  (if (>= n s)
                      (f (cdr lst) (1+ n) (cons (car lst) acc))
                      (f (cdr lst) (1+ n) acc))
                  (reverse acc))))
    (f lst 1 nil)))

(defun rotate (lst n)
  (let ((offset (mod (+ n (length lst)) (length lst))))
    (multiple-value-bind (l1 l2) (split lst offset)
      (append l2 l1))))

(defun remove-at (lst remove-n)
  (labels ((f (lst n acc)
             (if lst
                 (if (= n remove-n)
                     (f (cdr lst) (1+ n) acc)
                     (f (cdr lst) (1+ n) (cons (car lst) acc)))
                 acc)))
    (reverse (f lst 1 nil))))

(defun insert-at (el lst insert-n)
  (labels ((f (lst n acc)
             (if lst
                 (if (= n insert-n)
                     (f (cdr lst) (1+ n) (cons (car lst) (cons el acc)))
                     (f (cdr lst) (1+ n) (cons (car lst) acc)))
                 acc)))
    (reverse (f lst 1 nil))))

(defun range (start end)
  (let ((step (if (< start end) 1 -1)))
    (labels ((f (n acc)
               (if (= n (+ end step))
                   acc
                   (f (+ n step) (cons n acc)))))
      (reverse (f start nil)))))

(defun rnd-select (lst n)
  (labels ((f (lst n acc)
              (if (and lst (> n 0))
                  (let ((rnd (random (length lst))))
                    (f (remove-at lst (1+ rnd)) (1- n) (cons (nth rnd lst) acc)))
                  acc)))
    (f lst n nil)))

(defun lotto-select (n max)
  (rnd-select (range 1 max) n))

(defun rnd-permu (lst)
  (rnd-select lst (length lst)))

(defun bc (n k)
  (cond
    ((= n k) 1)
    ((= k 0) 1)
    (t (+ (bc (1- n) (1- k)) (bc (1- n) k)))))

(defun combination (n lst)
  (let* ((count (length lst))
         (max (bc count n)))
    (labels ((f (acc)
               (if (< (length acc) max)
                   (let ((permu (rnd-select lst n)))
                     (if (find permu acc)
                         (f acc)
                         (f (cons permu acc))))
                   acc)))
      (f nil))))

