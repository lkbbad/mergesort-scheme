#lang racket

(define (odd mylist)
  (cond
    ((empty? mylist) '())
    ((empty? (rest mylist)) (list (first mylist)))
    (else (cons (first mylist) (odd (rest (rest mylist)))))
  )
)

(define (even mylist)
  (cond
    ((empty? mylist) '())
    ((empty? (rest mylist)) '())
    (else (cons (first (rest mylist)) (even (rest (rest mylist)))))
  )
)

(define (split mylist)
  (cons (odd mylist) (cons (even mylist) '()))
 )


(define (merge mylist1 mylist2)
    (if (empty? mylist1) mylist2
    (if (empty? mylist2) mylist1
    (if (< (first mylist1) (first mylist2))
        (cons (first mylist1) (merge (rest mylist1) mylist2))
        (cons (first mylist2) (merge (rest mylist2) mylist1))
    )
  )
 )
)

(define (mergesort mylist)
  (cond
    ((empty? mylist) mylist)
    ((empty? (rest mylist)) mylist)
    (else (merge
        (mergesort (first (split mylist))) (mergesort (first (rest (split mylist))))))
  )
)


(split '(5 3 6 7 8))
(merge '(1 4 5) '(2 3 5))
(mergesort '(1 5 7 8 9 0 343 56 4 2 12 0 4536 5))
(mergesort '(1 25 7 9 3))
(mergesort '())
(mergesort '(1))
