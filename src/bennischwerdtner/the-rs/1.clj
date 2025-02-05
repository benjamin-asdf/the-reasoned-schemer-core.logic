(ns bennischwerdtner.the-rs.1
  (:refer-clojure :exclude [==])
  (:require [clojure.string :as str]
            [clojure.core.logic :refer :all :as l]))

;; ----------------------------

l/succeed

l/s#

l/u#

(run* [q] l/u#)

(run* [q] (== q 10))

(run* [q]
  l/u#
  (== q 10))

(run* [q]
  s#
  (== q 10))

(run* [q]
  s#
  (== q 'corn))



(run* [q]
  (let [x true]
    ;; (==  q x)
    (== x false)))


(run* [q]
  (let [x true]
    (== q x)
    (== q false)))


;; ----------------------

(run* [q]
  (fresh [x]
    (== true x)
    (== true q)))


(run* [q]
  (fresh [x]
    (== true x)
    (== true q)))



(run* [x]
  (let [x false]
    (fresh [x]
      (== true x))))


(run* [r]
  (fresh [x y]
    (==
     (cons x (cons y ()))
     r)))
