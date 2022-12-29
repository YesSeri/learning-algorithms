(defun bubblesort-rec (lst)
  (let* ((first (car lst))(second (cadr lst)))
  (cond ((null first) '())
	((null second) lst)
	((< first second) (cons first (bubblesort-rec (cdr lst))))
	(t (bubblesort-rec (cons second (cons first (cddr lst))))))))
(defun bubblesort-iter (lst len)
  (if (= 0 len) lst
    (bubblesort-iter (bubblesort-rec lst) (1- len))))

(defun bubblesort (lst)
  (bubblesort-iter lst (length lst)))

(bubblesort '(1 2 0 8 3))
(bubblesort '())
(cons 1 '(2))
