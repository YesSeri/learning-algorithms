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

(defun merge-sort-tests ()
  (and
   (compare-lists (merge-sort '(1 -1 4 2 5)) '(-1 1 2 4 5))
   (compare-lists (merge-sort '(1)) '(1))
   (compare-lists (merge-sort '()) '())
   (compare-lists (merge-sort '(2 1)) '(1 2))
   (let* ((s (split-list '(1 2 3 4 5)))(left (car s))(right (cadr s)))
     (compare-lists left '(1 2 3))
     (compare-lists right '(4 5))
   )
   (compare-lists (split-list '(1)) '(1))
   (compare-lists (split-list '()) '())
   (let* ((s (split-list '(1 2)))(left (car s))(right (cadr s)))
     (compare-lists left '(1))
     (compare-lists right '(2)))))


;; I made these two functions for my tests.
(defun compare-lists-rec (lst1 lst2)
  (let ((x (car lst1))(y (car lst2)))

    (if (and (null lst1)(null lst2)) t
    (and (= x y)(compare-lists (cdr lst1)(cdr lst2))))))

(defun compare-lists (lst1 lst2)
  (if (= (length lst1)(length lst2))
      (compare-lists-rec lst1 lst2)))
