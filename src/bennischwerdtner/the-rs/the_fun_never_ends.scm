
(define (var name) (vector name))
(define (var? x) (vector? x)) 

(define (walk v s)
  (let ((a (and (var? v) (assv v s))))
    (cond ((pair? a) (walk (cdr a) s))
          (else v))))

(assv 'x '((x . v)))

(define
  (ext-s x v s)
  (cond ((occurs? x v s) #f)
        (else (cons '(,x . ,v) s))))

(ext-s 'foo 'bar '())

(define
  (occurs? x v s)
  (let ((v (walk v s)))
    (cond
     ((var? v) (eqv? v x))
     ((pair? v)
      (or
       (occurs? x (car v) s)
       (occurs? x (cdr v) s)))
     (else #f))))

(define
  (occurs? x v s)
  (let ((v (walk v s)))
    (cond ((var? v) (eqv? v x))
          ((pair? v)
           (or (occurs? x (car v) s)
               (occurs? x (cdr v) s)))
          (else #f))))


(occurs? 'x 'x '())

;; (occurs? x x '())















;; ------------------------------


(define (nevero)
  (lambda (s)
    (lambda ()
      ((nevero) s))))


(((nevero) 'a))



(let
    ((s∞
      ((disj2
        (≡ 'olive x) (nevero))
       empty-s)))
  s∞)

;; ('((,x . 'olive)) . (lambda () ((nevero) <captured-s>)) )

