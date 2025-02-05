(ns bennischwerdtner.the-rs.the-fun-never-ends
  (:refer-clojure :exclude [==])
  (:require [clojure.string :as str]
            [clojure.core.logic :refer :all :as l]))

;; ----------------------------------

;; l/succeed

(defn alwayso []
  (conde
   [l/succeed]
   [(alwayso)]))

(run 1 [q] (alwayso))

(run 1 [q]
  (conde
   [l/succeed]
   [(alwayso)]))

;; (run* [q] (alwayso))

;; ---------------------------

(run 5 [q]
  (alwayso))

(run 5 [q]
  (== q 'onion)
  (alwayso))

(run 1 [q]
  (alwayso)
  l/fail)




(run 1 [q]
  (== 'garlic q)
  l/succeed
  (== 'onion q))

(run 1 [q]
  (== 'garlic q)
  (alwayso)
  (== 'onion q))

;; ---------------------

(run 1 [q]
  (conde
   [(== 'garlic q)
    (alwayso)]
   [(== 'onion q)])
  (== 'onion q))


(run 2 [q]
  (conde
   [(== 'garlic q)
    (alwayso)]
   [(== 'onion q)])
  (== 'onion q))

(run 2 [q]
  (fresh []
    (== 'garlic q)
    (alwayso))
  (== 'onion q))

(run 5 [q]
  (conde
   [(== 'garlic q) (alwayso)]
   [(== 'onion q) (alwayso)])
  (== q 'onion))

'(onion onion onion onion onion)


(defn nevero [s]
  (fn [] (nevero s)))

(defn nevero []
  (fn [s]
    (fn [] ((nevero) s))))


(run 1 [q]
  l/fail
  nevero)

(run 2 [q]
  (conde
   [(nevero)]
   [l/succeed]
   [l/succeed]))

;; -------------------

(run 2 [q]
  (conde
   [l/succeed]
   [nevero]))

;; -----------------

(run 1 [q]
  (conde
   [l/succeed]
   [nevero])
  l/fail)

(run 5 [q]
  (conde
   [nevero]
   [(alwayso)]
   [nevero]))

(run 6 [q]
  (conde
   [(== 'spicy q) nevero]
   [(== 'hot q) nevero]
   [(== 'appple q) (alwayso)]
   [(== 'cider q) (alwayso)]))


(defn very-recursiveo
  []
  (conde
   [nevero]
   [(very-recursiveo)]
   [(alwayso)]
   [(very-recursiveo)]
   [nevero]))

(count (run (long 1e6) [q]
         (very-recursiveo)))


(defn occurs? [x v s]

  )
