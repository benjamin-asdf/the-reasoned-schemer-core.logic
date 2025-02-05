(define (member? x l)
  (cond
   ;; empty -> false
   ((null? l) #f)
   ;;
   ;; the first element is equal to l
   ;; true
   ((equal? (car l) x) #t)
   ;; else
   ;; recurse with the rest of l
   (#t (member? x (cdr l)))))

(member? 10 '(10 11))
(member? 100 '(10 11))
(member? 'olive '(virgin olive oil))

(define list? )
(list? '(1))
(list? '(1 . nil))
(list? '(1 2 ))

(define (proper-member? x l)
  (cond
   ((null? l) #f)
   ((equal? x (car l))
    (list? (cdr l)))
   (#t (proper-member? x (cdr l)))))

(proper-member? 'tofu '(tofu))
(proper-member? 'tofu '(tofu . ()))
(proper-member? 'tofu '(tofu . a))




