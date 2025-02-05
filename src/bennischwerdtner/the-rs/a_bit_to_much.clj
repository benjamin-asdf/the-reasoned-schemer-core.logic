(ns bennischwerdtner.the-rs.a-bit-to-much
  (:refer-clojure :exclude [==])
  (:require [clojure.string :as str]
            [clojure.core.logic :refer :all :as l]))

(defn bit-xoro [x y r]
  (conde
   [(== 0 x) (== 0 y) (== 0 r)]
   [(== 0 x) (== 1 y) (== 1 r)]
   [(== 1 x) (== 0 y) (== 1 r)]
   [(== 1 x) (== 1 y) (== 0 r)]))
