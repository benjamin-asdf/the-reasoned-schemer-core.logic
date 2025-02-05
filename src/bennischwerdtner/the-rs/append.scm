
(append '(a) 'a)

(append '(a c l) '(b))

(define (append l t)
  (cond
   ((null? l) t)
   (#t
    (cons
     (car l)
     (append (cdr l) t)))))

(append '(a b c) '(d e))
'(a b c d e)

(append '(a b c) '())

(append '() '(d e))

(append 'a '(d e))
(append '(d e) 'a)






