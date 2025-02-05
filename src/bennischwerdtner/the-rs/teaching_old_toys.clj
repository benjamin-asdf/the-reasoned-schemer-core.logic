(ns  bennischwerdtner.the-rs.teaching-old-toys
  (:refer-clojure :exclude [==])
  (:require [clojure.string :as str]
            [clojure.core.logic :refer :all :as l]))

(first '(grape raisin pear))

(l/run* [q]
  (l/firsto
   '(a c o r n)
   q))

(l/run* [q]
  (l/firsto '(a c o r n) 'a))

(l/run* [r]
  (l/fresh [x y]
    (l/firsto (list r y) x)
    (== 'pear x)))


(caro a-list something)

(l/run* [q]
  (l/conso q (list 2 3) (list 1 2 3)))

(l/run* [q]
  (== '(1 2 3) (l/lcons q '(2 3))))

(l/run* [q]
  (==
   (l/lcons q '(2 3))
   '(1 2 3)))

(defn my-caro [p a]
  (fresh [d]
    (== p (l/lcons a d))))

(l/run* [q]
  (my-caro '(1 2 3) q))

(l/run* [q]
  (firsto '(1 2 3) q))

;; -----------

(vector
 (first '[grape raisin pear])
 (first '[a b c]))

(l/lcons
 (first '[grape raisin pear])
 (first [(l/llist 'a nil)]))


(l/run* [r]
  (fresh [x y]

    (firsto '[grape raisin pear] x)

    (firsto
     (l/llist
      (l/llist 'a nil)
      (l/llist 'b nil)
      (l/llist 'c nil))
     y)


    (== (l/lcons x y) r)))


(rest '(grape raisin pear))


(first
 (rest (rest '(a c o r n))))

(l/run* [q]
  (l/resto '(1 2 3) q))

(l/run* [r]
  (l/fresh [v]
    (resto '(a c o r n) v)
    (fresh [w]
      (resto v w)
      (firsto w r))))

(l/run* [r]
  (l/matche
   ['(a c o r n)]
   ([[_ _ r . _]])))

(defn my-cdro [the-list the-rest]
  (fresh [a]
    (== the-list
        (l/lcons a the-rest))))

(l/run* [r]
  (my-cdro '(1 2 3) r))

(l/run* [r]
  (my-cdro '(1 2 3) r)
  (l/resto '(1 2 3) r))

(l/defne my-cdro
  [xs xr]
  ([[_ . xr] xr]))

;; ----------------



(l/lcons
 (rest '(grape raisin pear))
 (first '((a) (b) (c))))

(l/run* [r]
  (l/fresh [x y]
    (resto '(grape raisin pear) x)
    (firsto '((a) (b) (c)) y)
    (== (l/lcons x y) r)))


;; (run* r
;;   (fresh (x y)
;;     (cdro '(grape raisin pear) x)
;;     (caro '((a) (b) (c)) y)
;;     (≡ (cons x y) r)))



(l/run* [q]
  (resto
   '(a c o r n)
   '(c o r n)))

(l/run* [x]
  (resto
   '(c o r n)
   ;; x
   (l/llist x 'r 'n nil)))


(l/run* [x]
  (l/matche
   ['(c o r n)]
   ([[_ . [x 'r 'n]]])))


;; ---------------------

(run* l
  (fresh (x)
    (cdro l '(c o r n))
    (caro l x)
    (≡ 'a x)))

(l/run* [l]
  (fresh [x]
    (resto l '(c o r n))
    (firsto l x)
    (== 'a x)))
;; ((a c o r n))

(l/run* [l]
  (l/conso '(a b c) '(d e) l))

(l/run* [l]
  (l/conso 1 '(a b c) l))

(run* [x]
  (l/conso
   x
   '(a b c)
   '(d a b c)))

;; ------------

(run*
  r
  (fresh (x y z) (≡ ‘ (e a d x) r) (conso y ‘ (a z c) r)))


(run* [r]
  (fresh [x y z]
    (== (l/llist 'e 'a 'd x) r)
    (l/conso y (l/llist 'a z 'c) r)))

(run* [r x y z]
  (== (l/llist 'e 'a 'd x) r)
  (l/conso y (l/llist 'a z 'c) r))

'([
  ;; r:
  (e a d . c)
  ;; x
  c
  ;; y
  e
  ;; z
  d])

(run* [r x y z]
  (l/matche [r] ([['e 'a 'd  x]]))
  (l/matche [r] ([[ y 'a  z 'c]])))


;; ----------------------

(run* x
  (conso x ‘ (a x c) ‘ (d a x c)))

(run* [x]
  (l/conso x
           (l/llist    'a x 'c nil)
           (l/llist 'd 'a x 'c nil)))

(run* [x]
  (l/matche
   [[  x 'a x 'c]]
   ([['d 'a x 'c]])))

;; -------------------------

(run* l (fresh (x) (≡ ‘ (d a x c) l) (conso x ‘ (a x c) l)))


(run* [l]
  (fresh [x]
    (==        (l/llist 'd 'a x 'c nil)     l)
    (l/conso x (l/llist    'a x 'c nil)     l)))
;; ((d a d c))

;; firsto, resto

(defn my-conso [element lst output-list]
  (fresh []
    (firsto output-list element)
    (resto  output-list lst)))

(defn my-conso-= [element lst    output-list]
  (== (l/llist    element lst)   output-list))

(defn λ-cons
  [a b]
  {:first (fn [] a)
   :rest  (fn [] b)})

(l/defne
  my-conso
   [element lst output-list]
  ([element lst [element . lst]]))

(defn my-conso
  [element lst output-list]
  (conde [(firsto output-list element)
          (resto output-list lst)]))

(run* [q]
  (my-conso 'x (l/llist 1 2 3 nil) q))

(run* [q]
      (my-conso   'x (l/llist 1 2 3 nil) q)
      (my-conso-= 'x (l/llist 1 2 3 nil) q))

(run* [q]
  (firsto '(1 2 3) q))

;; (x 1 2 3)

(run* l
  (fresh (d t x y w)
    (conso w '(n u s) t)
    (cdro l t)
    ;; (w n u s)
    ;; l: (? w n u s)
    (caro l x)
    ;; l: (x w n u s)
    (≡ 'b x)
    ;; x: b
    ;; l: (b w n u s)
    (cdro l d)
    ;; d: (w n u s)
    (caro d y)
    ;; y: w
    ;; y: o
    ;; w: o
    ;; d:   (o n u s)
    ;; l: (b o n u s)
    (≡ 'o y)))

(run* [l]
  (fresh [d t x y w]
    (l/conso w (l/llist 'n 'u 's nil) t)
    (l/resto l t)
    (l/firsto l x)
    (l/== 'b x)
    (l/resto l d)
    (l/firsto d y)
    (l/== 'o y)))

;; -----------------
(run* [q]
  (l/emptyo '(grape raisin 'pear)))

(run* [q]
  (l/emptyo '()))

(run* [x]
  (l/emptyo x))

(defn my-emptyo [x]
  (== x '()))

;; ---------------------------

(lcons 'split 'pea)

;; `(split . ,x)

#_(lcons 'split x)

(defn pair? [x]
  (or
   (lcons? x)
   (and (coll? x) (seq x))))

(pair? '((split) pea))
(pair? '())
(pair? 'pair)
(pair? 'pear)
(pair? (lcons 'pear nil))
(pair? (lcons 'pear '()))
(first (lcons 'pear '()))
(rest  (lcons 'pear '()))

(= (lcons  '(split)             'pea)
   (lcons  (l/llist 'split nil) 'pea))

(l/run* [q]
  (== q (lcons '(split) 'pea))
  (firsto q '(split)))

(l/run* [q a]
  (== q (lcons (l/llist 'split nil) 'pea))
  (fresh [d]
    (firsto q d)
    (firsto d a)))


(run* r (fresh (x y) (≡ (cons x (cons y 'salad)) r)))

(run* [r]
  (fresh [x y]
    (== (lcons x (lcons y 'salad)) r)))

((_0 _1 . salad))

(defn my-pairo
  [p]
  (fresh [a d]
    (l/conso a d p)))

(run* [q]
  (== q (lcons 'a 'b))
  (my-pairo q))

(run* [q]
  (firsto q 10)
  (my-pairo q))

(run* [q]
  (fresh [d]
    (== q (lcons 10 d)))
  (my-pairo q))

(run* q (pairo (cons q q)))

(run* [q]
  (my-pairo (lcons q q)))

(run* [q]
  (my-pairo '()))

(run* [q]
  (my-pairo 'pair))

(run* [x]
  (my-pairo x))

(run* [r q]
  (== q (lcons r 10))
  (my-pairo q))

'(tofu)

'((tofu))

'tofu

'(e tofu)

'()

'(e . tofu)

;; (define
;;   (singleton? l)
;;   (cond
;;     ((pair? l)
;;      (null? (cdr
;;              l)))
;;     (else #f)))



;; I don't actually check if this is a proper list
(defn singleton? [l]
  (and (seq? l)
       (empty? (rest l))))

(.-d (l/lcons 1 2))

(l/resto (l/lcons 1 '()))


(l/run* [q]
  (l/resto (l/lcons 1 '()) '()))


(l/run* [q r]
  (== r (.-d (l/lcons 1 q))))

(singleton? '((a) (a b) c))

(l/lcons 1 2)
[1 2]
(list 1 2 nil)

(l/lcons 1 2)
'(1 . 2)
(l/lcons 1 (l/lcons 2 nil))
'(1 2)

(singleton? (l/llist 'pea '()))

(singleton? (l/llist 'sauerkraut '()))

;; is correct?
;; says there is an a that is the single element of an
;; lcons l, where the second elm is '()
(defn singletono [l]
  (fresh [a]
    (== (l/lcons a '()) l)))

;; ------------------

(defn pairo [l]
  (fresh [a d]
    (== (l/lcons a d) l)))

(defn singletono
  [l]
  (fresh []
    (pairo l)
    (fresh [d]
      (l/resto l d)
      (l/emptyo d))))

(defn singletono
  [l]
  (fresh [d]
    (l/resto l d)
    (l/emptyo d)))

(l/run* [q]
  (singletono q)
  (l/firsto q 10))

(defn caro
  "A relation where l is a collection, such that a is the first of l"

  [lst first-elm]
  ;; (l/conso )
  ;; (fresh [d]
  ;;   (== (l/lcons first-elm d) lst))
  (fresh [d]
    (l/conso first-elm d lst)))

(defn cdro
  "A relation where l is a collection, such that d is the rest of l."
  [l d]
  ;; (l/conso )
  ;; (fresh [d]
  ;;   (== (l/lcons first-elm d) lst))
  ;; (fresh [a]
  ;;   (== (l/lcons a d) l))
  (fresh [a]
    (l/conso a d l)))

(l/run* [q]
  (l/firsto q 10))

(l/run* [q]
  (l/firsto q 10)
  (caro q 11)
  (cdro q '(a b c)))

;; ------------------------
