(defun split-list (lst &optional lst2)
"
Splits a list into two seperate lists.
This is what it returns for different inputs.
'() -> '()
'(1) -> '(1)
'(1 2) -> '((1)(2))
'(1 2 3) -> '((1 2)(3))
"
  (cond
   ((null lst)
    (if (null lst2) '() lst2))
   ((or
     ;; Even amount of elements
     (= (length lst)(length lst2))
     ;; Uneven amount of elements
     (= (1+ (length lst))(length lst2)))
    (list (reverse lst2) lst))
   (t (split-list (cdr lst)(cons (car lst) lst2)))))
(split-list '(1 2 3 4 5))
(split-list '(1))
(split-list '())
(split-list '(1 2))

(defun merge-sort (lst)
  (cond ((null lst) '())
	((null (cdr lst)) lst)
	(t
	 (let* (
		(s (split-list lst))
		(left (car s))
		(right (cadr s)))
	   (merge-fn (merge-sort left) (merge-sort right))))))

		
(defun merge-fn (left right)
  (if (or (null left)(null right))(append left right)
  (let ((l-first (car left))
	(r-first (car right)))
    (if (< l-first r-first)
	(cons l-first (merge-fn (cdr left) right))
      	(cons r-first (merge-fn  left (cdr right)))))))

(merge-sort '(1 3 5 6 2 4 5 6 1 0 9 -1))
(merge-sort '())
(merge-sort '(1))
(merge-sort '(1 2))
