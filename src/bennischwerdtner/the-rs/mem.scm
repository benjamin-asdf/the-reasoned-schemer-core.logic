

(define (mem x l)
  (cond
   ((null? l) #f)
   ((equal? (car l) x) l)
   (#t (mem x (cdr l)))))

;; is the same as member? but returns the list

(mem 'fig '(roll okra fig beet roll pea))
'(fig beet roll pea)

(mem 'fig '(roll okra beet beet roll pea))

;; and then that is (roll pea) 
(mem 'roll
     ;; that is fig...... 
     (mem 'fig '(roll okra fig beet roll pea)))
'(roll pea)



