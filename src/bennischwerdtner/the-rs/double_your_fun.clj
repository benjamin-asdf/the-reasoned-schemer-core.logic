(ns bennischwerdtner.the-rs.double-your-fun
  (:refer-clojure :exclude [==])
  (:require [clojure.string :as str]
            [clojure.core.logic :refer :all :as l]))

(defn appendo
  "A relation where `out` is the concatenation of `l` and `t`.
  `l` and `t` are lists."
  [l t out]
  (conde
   [(l/emptyo l) (== out t)]
   [(fresh [res]
      (fresh [d]
        (l/resto l d)
        (appendo d t res))
      (fresh [a]
        (l/firsto l a)
        (l/conso a res out)))]))


;; ----------

(defn appendo
  "A relation where `out` is the concatenation of `l` and `t`.
  `l` and `t` are lists."
  [l t out]
  (conde
   [(l/emptyo l) (== out t)]
   [(fresh [res d a]
      (l/resto l d)
      (appendo d t res)
      (l/firsto l a)
      (l/conso a res out))]))

(defn appendo
  "A relation where `out` is the concatenation of `l` and `t`.
  `l` and `t` are lists."
  [l t out]
  (conde
   [(l/emptyo l) (== out t)]
   [(fresh [res d a]
      (l/conso a d l)
      (appendo d t res)
      (l/conso a res out))]))

(defne appendo
  [l t out]
  ([[] t t])
  ([[a . d] t out]
   (fresh [res]
     (l/conso a res out)
     (appendo d t res))))

(defne
  appendo
  [l t out]
  ([[] t t])
  ([[a . d] t [a . res]]
   (appendo d t res)))

(defne appendo
  "A relation where x, y, and z are proper collections,
  such that z is x appended to y"
  [x y z]
  ([() _ y])
  ([[a . d] _ [a . r]] (appendo d y r)))
;; I first destructure and then make the append call



#_(defne
    appendo
    "A relation where `l`, `y`, `out` are proper collections,
such that out is the concatenation of l and y."
    [l y out]
    ([() _ y])
    ([[a . d] _ [a . r]]
     (appendo d y r)))

(l/run* [q]
  (appendo [1 2] [3] q)
  ;; (appendo [1 2 3] [] q)
  )

;; -----------------------

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

(defn loso
  [l]
  (conde
   [(l/emptyo l)]
   [(fresh [a d]
      (l/conso a d l)
      (singletono a)
      (loso d))]))

(defne
  loso
  "A relation where `l` is a list of singletons."
  [l]
  ([()])
  ([[[_] . d]]
   (loso d)))


(run* [q]
  (== q 10)
  (loso [[q]]))

(run* [q]
  (== q 10)
  (loso [[q 11]]))

(defn listo
  "A relation where `l` is a proper list."
  [l]
  (fresh [a]
    (conde
     [(== (l/lcons a '()) l)]
     [(fresh [d]
        (l/resto l d)
        (listo d))])))

(defne listo
  [l]
  ([()])
  ([[_ . d]]
   (listo d)))

(defn lolo
  "A relation where `l` is a list of lists."
  [l]
  (conde
   [(l/emptyo l)]
   [(fresh [a d]
      (l/conso a d l)
      (listo a)
      (lolo d))]))

(defne lolo
  "A relation where `l` is a list of lists."
  [l]
  ([()])
  ([[a . d]]
   (listo a)
   (lolo d)))

(run* [q]
  (lolo [[1 2] [1]]))


(defn proper-membero
  [x l]
  (conde [(l/firsto l x)
          (fresh [d] (l/resto l d) (listo d))]
         [(fresh [d] (l/resto l d) (proper-membero x d))]))

(defn proper-membero
  [x l]
  (fresh [d]
    (l/resto l d)
    (conde
     [(l/firsto l x)
      (listo d)]
     [(proper-membero x d)])))

(run* [q]
  (proper-membero q (llist 1 2 3 nil)))

(run 1 [q]
  (proper-membero q (llist 1 2 3 nil)))

;; ---------------------------

(defne
  proper-membero
  "A relation where `x` is a member of `l`.
`l` is a proper list.
"
  [x l]
  ([_ [x . d]]
   (listo d))
  ([_ [_ . d]]
   (listo d)
   (proper-membero x d)))


(run 3 [x]
  (fresh [y z]
    (appendo x y z)))

(run 6 [y]
  (fresh [x z]
    (appendo x y z)))


(run 6 [x y z]
  (appendo x y z))

'([() _0 _0]
 [(_0) _1 (_0 . _1)]
 [(_0 _1) _2 (_0 _1 . _2)]
 [(_0 _1 _2) _3 (_0 _1 _2 . _3)]
 [(_0 _1 _2 _3) _4 (_0 _1 _2 _3 . _4)]
  [(_0 _1 _2 _3 _4) _5 (_0 _1 _2 _3 _4 . _5)])


(run* [x]
  (appendo
   (llist 'cake nil)
   (llist 'tastes 'yummy nil)
   x))

(run* [x]
  (fresh [y]
    (appendo
     (llist 'cake-and-ice y nil)
     (llist 'tastes 'yummy nil)
     x)))

;; -------------------------

(run* [x]
      (fresh
        [y]
        (appendo (llist 'cake-and-ice 'cream nil) y x)))

(run 1 [x]
  (fresh [y]
    (appendo
     (llist 'cake-and-ice y)
     (llist 'd 't nil)
     x)))

(run 5 [x]
  (fresh [y]
    (appendo
     (llist 'cake-and-ice y)
     (llist 'd 't nil)
     x)))

'((cake-and-ice d t)
  (cake-and-ice _0 d t)
  (cake-and-ice _0 _1 d t)
  (cake-and-ice _0 _1 _2 d t)

  (cake-and-ice _0 _1 _2 _3 d t)

  )


(run 5 [y]
  (fresh [x]
    (appendo
     (llist 'cake-and-ice y)
     (llist 'd 't nil)
     x)))

'(cake-and-ice _0 _1 _2)

'(cake-and-ice _0 _1 _2 _3 d t)

(run 5 [x]
  (fresh [y]
    (appendo
     (llist 'cake-and-ice y)
     (llist 'd 't y)
     x)))

;; 👈 this was a clever counter example, where the second list
;; shares structure with the first

'((cake-and-ice d t)
  (cake-and-ice _0 d t _0)
  (cake-and-ice _0 _1 d t _0 _1)
  (cake-and-ice _0 _1 _2 d t _0 _1 _2)
  (cake-and-ice _0 _1 _2 _3 d t _0 _1 _2 _3))

(run* [x]
  (fresh [z]
    (appendo
     (llist 'cake-and-ice 'cream nil)

     (llist 'd 't z)
     x)))

'((cake-and-ice cream d t . _0))

;; because we don't introduce fresh variables for
;; the rest of the second list.
;; the second list just stays.


;; ------------------

(run 6 [x]
  (fresh [y]
    (appendo x y (llist 'cake-and-ice 'd 't nil))))
;; x is all the prefixes of the list


'(() (cake-and-ice) (cake-and-ice d) (cake-and-ice d t))
;; and y accorindingly.......

(run 6 [x y]
  (appendo x y (llist 'cake-and-ice 'd 't nil)))

'([() (cake-and-ice d t)]
 [(cake-and-ice) (d t)]
 [(cake-and-ice d) (t)]
 [(cake-and-ice d t) ()])


(run 6 [y]
  (fresh [x]
    (appendo x y (llist 'cake-and-ice 'd 't nil))))

'((cake-and-ice d t) (d t) (t) ())
;; all the suffixes


;; all the 'splits'
;;

(run 7 [x y]
  (appendo x y
           (llist 'cake-and-ice 'd 't nil)))
;; since there is a nil here, it finishes

'([() (cake-and-ice d t)]
  [(cake-and-ice) (d t)]
  [(cake-and-ice d) (t)]
  [(cake-and-ice d t) ()])

;; not sure why it works in core.logic, 🤷


;; ah, I had the goals swapped already

(run 7 [x y q]
  (appendo x y
           (llist 'cake-and-ice 'd 't q)))

'([() (cake-and-ice d t . _0) _0]
 [(cake-and-ice) (d t . _0) _0]
 [(cake-and-ice d) (t . _0) _0]
 [(cake-and-ice d t) _0 _0]
 [(cake-and-ice d t _0) _1 (_0 . _1)]
 [(cake-and-ice d t _0 _1) _2 (_0 _1 . _2)]
 [(cake-and-ice d t _0 _1 _2) _3 (_0 _1 _2 . _3)])


;; +--------------------------------------------------------+
;; |                                                        |
;; |             The First Commandment                      |
;; |                                                        |
;; |                                                        |
;; |                                                        |
;; |    Within each sequence of goals, move non-recursive   |
;; |    goals before recursive goals.                       |
;; |                                                        |
;; +--------------------------------------------------------+


(defn swappendo
  ""
  [l t out]
  (conde
   [(fresh [res d a]
      (l/conso a d l)
      (l/conso a res out)
      (swappendo d t res))]
   [(l/emptyo l) (== out t)]))

;; hm, will always be an infinite tail? (no)

(run 7 [x y]
  (swappendo x y
             (llist 'cake-and-ice 'd 't nil)))

'([() (cake-and-ice d t)]
 [(cake-and-ice) (d t)]
 [(cake-and-ice d) (t)]
 [(cake-and-ice d t) ()])



;; +--------------------------------------------------------+
;; |                                                        |
;; |;              The Law of Swapping conde Lines          |
;; |;                                                       |
;; |; Swapping two conde lines does not affect the values   |
;; |; contributed by conde.                                 |
;; +--------------------------------------------------------+


;; swappendo sounds like a Harry Potter spell. Love it!

(defn unwrap [coll]
  (cond
    (seqable? coll)
    (unwrap (first coll))
    :else
    coll))

(unwrap [[[[[:pizza]]]]])
:pizza

(unwrap [[[[[:pizza :pie] :with] :garlic]]])

:pizza

;; I need pairo

(defn pairo
  [p]
  (fresh [a d]
    (l/conso a d p)))

;; 🪄
(defn unwrapo
  "A relation where `l` might be a list and `out`
  is the first unnested item that is not a list.
  "
  [l out]
  (conde
   [(fresh [a]
      (l/firsto l a)
      (unwrapo a out))]
   ;; wait this is not correct (becuase it's disjunct, both is allowed)
   ;; but let's continue in the book, it's meant to be
   [
    ;;
    (== out l)]))

(l/defne
  unwrapo
  "A relation where `l` might be a list and `out`
  is the first unnested item that is not a list."
  [l out]
  ([[a . _] _]
   (unwrapo a out))
  ([_ l]))

(run* [x]
  (unwrapo
   (llist (llist (llist (llist 'pizza nil) nil) nil) nil)
   x))

;; it kinda finds the non recursive solution first, then the recursive
;; solutions

;;            DON’T PANIC
;; Thank you, Douglas Adams (1952–2001).

;; Seriously, I love those books

;; read or listen to Hitchickers Guide to The Galaxy if you have not
;; ----------------------------------------------------------------

(run 1 [x]
  (unwrapo x 'pizza))
;; (is basically this)

(run 1 [x]
  (unwrapo (llist (llist x nil) nil) 'pizza))
;; yea, the only way that the relation works is when x is pizza.


(run 5 [x]
  (unwrapo x 'pizza))
;; here, we have all possible lists, where the first nested car is pizza

'(pizza
 (pizza . _0)
 ((pizza . _0) . _1)
 (((pizza . _0) . _1) . _2)
 ((((pizza . _0) . _1) . _2) . _3))


(run 5 [x]
  (unwrapo x (llist (llist 'pizza nil) nil)))

;; wait, ah the list is the out
;; yea, everything up from ((pizza)) kinda

;; what is the lazy seq thing in core.logic here ?
;; it should say ((pizza))
;;
;; I think it is the object that is the outcome of
;; (llist (llist 'pizza nil) nil)



;; (((pizza))
;;  (clojure.lang.LazySeq@309e4e91 . _0)
;;  ((clojure.lang.LazySeq@309e4e91 . _0) . _1)
;;  (((clojure.lang.LazySeq@309e4e91 . _0) . _1) . _2) ((((clojure.lang.LazySeq@309e4e91 . _0) . _1) . _2) . _3))


;; works more obviously in this case:

(run 5 [x]
  (unwrapo x [[:pizza]]))

'([[:pizza]]
  ([[:pizza]] . _0)
  (([[:pizza]] . _0) . _1)
  ((([[:pizza]] . _0) . _1) . _2)
  (((([[:pizza]] . _0) . _1) . _2) . _3))

(defne unwrapo
  ""
  [l out]
  ([[a . _] _]
   (unwrapo a out))
  ([out _]))

;; except here we ask for 5 results
(run 5 [x]
  (unwrapo (llist (llist x nil) nil) 'pizza))


'(pizza
  (pizza . _0)
  ((pizza . _0) . _1)
  (((pizza . _0) . _1) . _2)
  ((((pizza . _0) . _1) . _2) . _3))

;; pizza list again

;; I just ate pasta


;; Would be the epic walktrhough of the book if you also eat everytime
;;












;; -------------------------------

(run 1 [x y]
  (== x 1)
  (== x y))

(defn boolo [x]
  (conde
   [(== x true)]
   [(== x false)]))

(run* [x y]
  (boolo x)
  (boolo y)
  ;; conjunction
  (conde
   [(== x true)
    (== y true)]))

(run* [x y]
  (boolo x)
  (boolo y)
  ;; disjunction
  (conde
   [(== x true)]
   [(== y true)]))

'([true true]
  [true true]
  [true false]
  [false true])

(run* [p q]
  (boolo p)
  (boolo q)
  ;; implication
  (conde
   ;; P -> Q
   [(== p true)  (== q true)]
   [(== p false) (== q false)]
   [(== p false) (== q true)]))

'([true true]
  [false true]
  [false false])

(run* [p q]
  (boolo p)
  (boolo q)
  ;; biconditional
  (== p q))

'([true true]
 [false false])

(run* [p]
  (boolo p)
  ;; (boolo out)
  ;; negation
  (!= p true)
  ;; (conde
  ;;  [(== p true) (== out false)]
  ;;  [(== p false) (== out true)])
  )
