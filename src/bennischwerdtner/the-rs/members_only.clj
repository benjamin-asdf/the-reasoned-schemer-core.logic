(ns bennischwerdtner.the-rs.members-only
  (:refer-clojure :exclude [==])
  (:require [clojure.string :as str]
            [clojure.core.logic :refer :all :as l]))

;; elephant (not a member? :( )
;; elephant

;; back:
;; giraffe
;; papagay
;; hippo

;; middle:
;; rhino
;; maybe he/she is a marmot

;; front:
;; papagay (2)
;; monkey
;; dog? what is he?
;; mammal because of the snaut

;;
;; friendly lion
;; tiger, or really fat cat
;;


;; ------------------------------------------------------
;; they have a lot of books there
;; eveyrbody is reading
;;
;; mommy elephant is carrying even more books around, as
;; reinforment
;; next batch I mean
;;
;;
;; some of them drink soda
;;
;; there is no wall, but a door in the air
;;
;; and elephant seems sad he is not allowed in.
;;
;; an exclusive book club?
;;

(defn memo [x l out]
  (conde
   ;; or == 'false' ?
   ;; ah, no drop this goal, because it should be 'false'
   ;; so just dont' succeed the goal. I forgot ye.
   ;; [(l/emptyo l) (== out '())]
   [(l/firsto l x)
    (== out l)]
   [(fresh [d]
      (l/resto l d)
      (memo x d out))]))

;; and now with core.logic

(defne
  memo
  ;; first draft docstring
  "A relation where `l` is a list, `x` is an element in `l` and
  `out` is the first suffix of `l` that contains `x` - as first item.
 "
  [x l out]
  ([_ [x . _] l])
  ([_ [_ . d] _]
   (memo x d out)))

(l/run* [q]
  (memo 'fig (llist 'pea (llist 'pea nil) nil) q))

;; that does not succeed

(l/run* [out]
  (memo 'fig (llist 'fig nil) out))
;; the list (fig)

;; that is the list (fig pea)
(l/run* [out]
  (memo 'fig (llist 'fig 'pea nil) out))
'((fig pea))

(run 1 [q]
  (== q 10))

(run* [r]
  (memo r
        (llist 'roll 'okra 'fig 'beet 'fig 'pea nil)
        (llist 'fig 'beet 'fig 'pea nil)))
'(fig)

(run* [x]
  (memo
   'fig
   (llist 'fig 'pea nil)
   (llist 'pea x nil)))

;; yea there just is nothing you can put into x that would make it work

(run* [out]
  (memo 'fig (llist 'beet 'fig 'pea nil) out))
'((fig pea))

;;
(run 1 [out]
  (memo 'fig (llist 'fig 'fig 'pea nil) out))
'((fig fig pea))

;; and now there are two possible ig
(run* [out]
  (memo 'fig (llist 'fig 'fig 'pea nil) out))

'((fig fig pea) (fig pea))

;; yea it's surprising because in procedural programing,
;; we would have stopped once the alg is done,
;; but alg is not done here, ... it's that there are values where the relationship
;; holds

;; because we try all possible goals, so basically we go into the
;; recursive conde branch
;;

;; conde allows multiple things to be true kinda

(run*
  [out]
  (fresh [x]
    (memo 'fig (llist 'a x 'c 'fig 'e nil) out)))

'((fig c fig e) (fig e))


(run 5 [x y]
  (memo 'fig (llist 'fig 'd 'fig 'e y) x))

;; ([(fig d fig e . _0) _0]
;;  [(fig e . _0) _0]
;;  [(fig . _0) (fig . _0)]
;;  [(fig . _0) (_1 fig . _0)]
;;  [(fig . _0) (_1 _2 fig . _0)])


([(fig d fig e . _0) _0]
 [(fig e . _0) _0]

 ;;  ah lol y became (fig . _0)

 ;; why is x then (fig . _0), it's kinda like they met at the bottom,
 ;; value gen vice
 [(fig . _0) (fig . _0)]
 [(fig . _0) (_1 fig . _0)]
 ;; ah and then y grows at the front with more values...
 [(fig . _0) (_1 _2 fig . _0)])

;; bottom line for me is that you don't really know so easily how the
;; logic engine is solving your query

;; yea, y grows to be all possible suffixes that contain 'fig


;; I think.... :
(
 ((fig d fig e . _0) _0)
 ((fig e . _0) _0)

 ;; hm upper 2 were right, that is where y is still fresh
 ;; then...

 ((fig d fig e fig) (fig))
 ((fig e fig) (fig))


 ;; I thought of that down here
 ((fig d fig e fig . _0) (fig . _0)))


(defn rembero
  [x l out]
  (conde
   [(l/emptyo l) (== '() out)]
   [(l/conso x out l)]
   [(fresh [a d res]
      (l/conso a d l)
      (l/conso a res out)
      (rembero x d res))]))

(defne
  rembero
  "A relation where `out` is a list where `x` is removed  from `l` maximally one time."
  [x l out]
  ([_ () ()])
  ([_ [x . out] _])
  ([_ [a . d] [a . res]]
   (rembero x d res)))

(run* [q]
  (rembero
   'pea
   (llist 'a 'b 'pea 'd 'pea 'e nil)
   q))

(run* [out] (rembero 'pea (llist 'pea nil) out))

(run* [out] (rembero 'foo (llist 'pea nil) out))

(run* [out]
  (rembero 'pea (llist 'pea 'pea nil) out))

(run* [out]
      (fresh [y z]
        (rembero
         y
         (llist 'a 'b y 'd z 'e nil)
         out)))
'((b a d _0 e)
  (a b d _0 e)
  (a b d _0 e)

  (a b d _0 e)

  (a b _0 d e)

  (a b e d _0)
  (a b _0 d _1 e))


(run* [out y z]
  (rembero
   y
   (llist 'a 'b y 'd z 'e nil)
   out))

'([(b a d _0 e) a _0]
   [(a b d _0 e) b _0]
   [(a b d _0 e) _1 _0]
   [(a b d _0 e) d _0]
   [(a b _0 d e) _0 _0]
   [(a b e d _0) e _0]
  [(a b _0 d _1 e) _0 _1])

;; ----------------------------

;; [x . out]
;;  y   ['d z 'e]
;;
;; y - fresh
;; out - ['d  z 'e]
;; out - [ y 'd 'e]    (input)
;;
;;
;; y - d
;; x - d
;;
;; ['d 'd 'e]
;; ['d 'd 'e]
;;

;; -------------------------


;;
;;
;; y - fresh
;;
;; l:
;; [y  'd z 'e]
;; [a . d]
;;
;; y: a
;; d: ['d z 'e]
;;
;; out:
;; [y 'd 'e]
;; [a . res]
;;
;; y: a: y
;; res: ['d 'e]
;;
;; ->
;;
;;
;; match:
;; [ x . out]
;; ['d   z 'e]
;;
;; x: y: 'd
;; out: [z  'e]
;;
;; match:
;; [z  'e]
;; ['d 'e]
;; z: 'd

;;
;; ['d 'd 'e]
;; out: ['d 'e]

(run* [y z]
  (rembero
   y
   (llist y 'd z 'e nil)
   (llist y 'd   'e nil)))

'([d d]
  [d d]
  [_0 _0]
  [e e])

;; -----------------------
;; ([_ [x . out] _])

(run 4 [y z w out]
  (rembero y (llist z w) out))

;; w
;;

'([_0 _0 _1 _1]
  [_0 _1 () (_1)]

  [_0 _1 (_0 . _2) (_1 . _2)]

  #_(rembero
     _0
     (_1 _0 . _2)
     (_1    . _2))

  [_0 _1 (_2) (_1 _2)]

  #_(rembero
     _0
     (_1 _2)
     (_1 _2))

  ;; l:
  ;; (_0 . out)
  ;; out:
  ;; _3

  ;;
  ;; _0
  ;; (_1 _2 _0 . _3)
  ;; (_1 _2 . _3)
  ;;
  ;; y: _0
  ;; z: _1
  ;; w: (_2 _0 . _3)
  ;; out: (_1 _2 . _3)

  )

(last
 (run 5 [y z w out]
   (rembero y (llist z w) out)))

'[_0
  _1
  (_2 _0 . _3)
  (_1 _2 . _3)]
