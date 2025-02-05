(ns bennischwerdtner.the-rs.1
  (:refer-clojure :exclude [==])
  (:require [clojure.string :as str]
            [clojure.core.logic :refer :all :as l]))


(l/run* [q]
  ;; (conde )
  (l/or* [(== q 1) (== q 2)]))

(l/run* [q]
  (l/and*
   [(== q 1)
    (== q 2)]))

(l/run* [q]
  (== q 1)
  (== q 2))

(l/run* [q]
        (conde [(== q 1) l/succeed]
               [;; conjunction:
                (== q 2) l/fail]
               [;; conjunction:
                (== q 3) l/succeed]))


(l/run* [q]
  (fresh [x]
    (== x q)
    (== x 10)))

(l/run* [q]
  (== [:a q] [:a 11]))

(l/run* [b q]
  (conde
   ;; disjunct:
   [(== b 20) (== [:a q] [:a 11])]
   [(== b 30)]))

;; outcomes:
;; ([20 11] [30 _0])

(l/run* [b q]
  (conde
   ;; disjunct:
   [(== b 20) (== [:a q] [:a 11])]
   [(== b 30) (== [:a :b] q)]))


;; conjunction:
;; (l/and* [g1 g2 g3])
;; --

;; disjunction:
;; (l/or* [g1 g2 g3])
;; --

(l/run* [q]
  (l/or* [(== q 10) (== q 11)])
  (== q 10))

(defn teacupo [x]
  (conde [(== x 'tea)]
         [(== x 'cup)]))

(l/defne
  teacupo
  [x]
  (['tea])
  (['cup]
   l/fail))

(l/run* [q]
  (== q 'cup)
  (teacupo q))

(l/defne
  teacupo
  [x]
  (['tea])
  (['cup]))

(l/defne
  container
  [x]
  (['cup])
  (['mug]))

(l/run* [q]
  (teacupo q)
  (container q))
