(ns bennischwerdtner.the-rs.seeing-old-friends-in-new-ways
  (:refer-clojure :exclude [==])
  (:require [clojure.string :as str]
            [clojure.core.logic :refer :all :as l]))

;; ------------------------


;; (define
;;   (list? l)
;;   (cond ((null? l) #t)
;;         ((pair? l) (list? (cdr l)))
;;         (#t #f)))

(defn pair? [x]
  (or
   (lcons? x)
   (and (coll? x) (seq x))))


;; this is not correct.
;; it is true for empty colls
(defn mylist?
  [l]
  (cond (= '() l) true
        (pair? l) (recur (rest l))
        true false))

;; -----------------------------------

(mylist? '((a) (a b) c))
(mylist? [])


;; ---------------------------

(defn listo
  "A relation where l is a proper list."
  [l]
  (fresh [a]
    (conde
     [(== (l/lcons a '()) l)]
     [(fresh [d]
        (l/resto l d)
        (listo d))])))

(l/run 6 [q]
  (fresh [x]
    (== q (l/llist 1 2 3 4 x 6 nil))
    (listo q)))

(l/run 6 [q]
  (fresh [x]
    (== q (l/llist 1 2 3 4 x 6 x))
    (listo q)))

(defn listo
  "A relation where l is a proper list."
  [l]
  (conde [(l/emptyo l)]
         [(fresh [d] (l/resto l d) (listo d))]))

;; (defrel
;;   (listo l)
;;   (conde
;;    ((nullo l))
;;    ((fresh
;;      (d)
;;      (cdro l d)
;;      (listo d)))))

;; ---------------------------------


(l/run* [x]
  (listo
   (llist 'a 'b x 'd nil)))

(l/run* [x]
  (listo
   (llist 'a 'b 'c 'd x)))

(l/run 1 [x]
  (listo
   (llist 'a 'b 'c 'd x)))

(l/run 10 [x]
  (== x '())
  (listo
   (llist 'a 'b 'c 'd x)))

(l/run 5 [x]
  (listo
   (llist 'a 'b 'c 'd x)))

;; (() (_0) (_0 _1) (_0 _1 _2) (_0 _1 _2 _3))

;;


;; lol?

(defn lolo
  [l]
  (conde [(l/emptyo l)]
         [(fresh [a] (l/firsto l a) (listo a))
          (fresh [d] (l/resto l d) (lolo d))]))

(run* [q]
  (fresh
    [x y]
    (lolo
     (llist
      (llist 'a 'b '())
      (llist x 'c '())
      (llist 'd y '())
      '()))))

(run* [q]
  (fresh [x y]
    (lolo
     (llist
      (llist 'a '())
      ;; (llist 'a '())
      '()))))

(run 1 [l] (lolo l))

(run 1 [q]
  (fresh [x]
    (lolo
     (llist (llist 'a 'b nil) x))))

(run 1 [x]
  (lolo
   (llist
    (llist 'a 'b nil)
    (llist 'c 'd nil)
    x)))

(run 5 [x]
  (lolo
   (llist
    (llist 'a 'b nil)
    (llist 'c 'd nil)
    x)))


;; ------------

(defn singletono [l]
  (fresh [a]
    (== (l/lcons a nil) l)))

(run* [q]
  (singletono q))


(defn loso
  [l]
  (conde
   [(l/emptyo l)]
   [(fresh [a]
      (l/firsto l a)
      (singletono a))
    (fresh [d]
      (l/resto l d)
      (loso d))]))

(l/defne singletono
  [l]
  ([[a]]))

(l/defne loso
  [l]
  (['()])
  ([[[a] . d]]
   (loso d)))

(run 1 [z]
  (loso
   (llist (llist 'g nil) z)))
(run 1 [z]
  (singletono
   (llist 'g 'c)))

(run 1 [z]
  (singletono
   (llist 'g nil)))

(run 1 [z]
  (loso
   (llist (llist 'g nil) nil)))

(run 5 [z]
  (loso
   (llist (llist 'g nil) z)))

(run 4
     [r]
     (fresh [w x y z]
       (loso
        (llist
         (llist 'g nil)
         (llist 'e w)
         (llist x y)
         z
         ))
       ;; (== (llist w (llist x y) z nil) r)
       (== [w [x y] z] r)
       ;; (== (llist w (llist x y) z nil) r)
       ))

;; ([() [_0 ()] ()]
;;  [() [_0 ()] ([_1])]
;;  [() [_0 ()] ([_1] [_2])]
;;  [() [_0 ()] ([_1] [_2] [_3])])

(let [smap (zipmap ['w 'x 'y 'z]
                   (nth (run 4
                          [r]
                          (fresh [w x y z]
                            (loso (llist (llist 'g nil)
                                         (llist 'e w)
                                         (llist x y)
                                         z))
                            (== [w x y z] r)))
                        3))]
  (clojure.walk/postwalk-replace
   smap
   [['g '()]
    ['e 'w]
    ['x 'y]
    'z]))

;; [[g ()] [e ()] [_0 ()] ([_1] [_2] [_3])]


;; (run
;;   4
;;   r
;;   (fresh
;;     (w x y z)
;;     (loso
;;      ‘((g) (e ▪ ,w) (,x ▪ ,y) ▪ ,z))
;;     (≡ ‘(,w (,x ▪ ,y) ,z) r)))

;; w: (g)
;; x: e
;; y: w: (g)

(run 3
  [out]
  (fresh
    [w x y z]
    (==
     (llist (llist 'g nil) (llist 'e w) (llist x y) z)
     out)
    (loso out)))

;; (((g) (e) (_0)) ((g) (e) (_0) [_1]) ((g) (e) (_0) [_1] [_2]))

(defn my-membero
  "A relation where x is an element of l."
  [x l]
  (conde
   [(fresh [a]
      (l/firsto l a)
      (== x a))]
   [(fresh [d]
      (l/resto l d)
      (my-membero x d))]))

(defn my-membero
  "A relation where x is an element of l."
  [x l]
  (conde
   [(l/firsto l x)]
   [(fresh [d]
      (l/resto l d)
      (my-membero x d))]))

(run* [q]
  (my-membero 'olive '(virgin olive oil)))

(run 1 [y]
  (my-membero y '(hummus with pita)))

(run 1 [y]
  (my-membero y '(with pita)))

(run 1 [y]
  (my-membero y '(pita)))

(run* [y]
  (my-membero y '()))

(run* [y]
  (my-membero y '(hummus with pita)))

#_(run* [y]
    (my-membero y l))

(run* [y]
  (my-membero y (llist 'pear 'grape 'peaches)))

(run* [y]
  (my-membero y (llist 'pear 'grape 'peaches nil)))


(run* [x]
  (my-membero 'e (llist 'pasta x 'fagioli nil)))

(run 1 [x]
  (my-membero 'e (llist 'pasta 'e x 'fagioli)))

(run 1 [x]
  (my-membero 'e (llist 'pasta x 'e 'fagioli)))

(run* [x]
  (my-membero 'e (llist 'pasta x 'e 'fagioli)))

(run* [x y]
  (my-membero 'e (llist 'pasta x y 'fagioli)))


;; ----------------------

(run* [q]
  (fresh [x y]
    (== (llist 'pasta x 'fagioli y nil) q)
    (my-membero 'e q)))

(run 1 [l]
  (my-membero 'tofu l))

;; has no value:
(run* [l]
  (my-membero 'tofu l))

(run 5 [l]
  (my-membero 'tofu l))
'((tofu . _0)
 (_0 tofu . _1)
 (_0 _1 tofu . _2)
 (_0 _1 _2 tofu . _3)
 (_0 _1 _2 _3 tofu . _4))


(run 5 [l]
  #_(fresh [d]
      (l/resto l d)
      (listo d))
  (listo l)
  (my-membero 'tofu l))

'((tofu)
 (tofu _0)
 (_0 tofu)
 (tofu _0 _1)
 (_0 tofu _1))

(defn proper-membero
  [x l]
  (conde [(l/firsto l x)
          (fresh [d] (l/resto l d) (listo d))]
         [(fresh [d] (l/resto l d) (proper-membero x d))]))

(run 12 [l]
  (proper-membero 'tofu l))

'((tofu)
 (tofu _0)
 (_0 tofu)
 (tofu _0 _1)
 (tofu _0 _1 _2)
 (_0 tofu _1)
 (tofu _0 _1 _2 _3)
 (tofu _0 _1 _2 _3 _4)
 (_0 _1 tofu)
 (_0 tofu _1 _2)
 (tofu _0 _1 _2 _3 _4 _5)
  (tofu _0 _1 _2 _3 _4 _5 _6))
