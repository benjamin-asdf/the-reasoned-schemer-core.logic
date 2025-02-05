
(define
  (rember x l)
  (cond ((null? l) '())
        ((equal? (car l) x) (cdr l))
        (#t
         (cons (car l)
               (rember x (cdr l))))))

(rember 'pea '(a b pea d pea e))



