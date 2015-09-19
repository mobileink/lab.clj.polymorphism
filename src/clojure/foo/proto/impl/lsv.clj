(ns foo.proto.impl.lsv
  "PFoo implementation common to lists, sets, vectors"
  (:refer-clojure :exclude [count])
  (:require [clojure.tools.logging :as log :only [debug info]]))

(println "loading foo.proto.impl.lsv")

(defn l-or-v-foo?
  [datum]
  (log/info "l-or-v-foo?")
  (some? (some #{:foo} datum)))

(defn m-or-s-foo?
  [datum]
  (log/info "m-or-s-foo?")
  (contains? datum :foo))

(defn count
  "count the int components of datum"
  [datum]
  (log/info "count")
  (clojure.core/count (filter integer? datum)))

(defn sum
  "sum the int components of datum"
  [datum]
  (log/info "sum")
  (reduce + (filter integer? datum)))

(defn mult
  "multiply the int components of datum"
  [datum]
  (log/info "mult")
  (reduce * (filter integer? datum)))

(defn mean-arith
  "compute the arithmetic mean of the int components of datum"
  [datum]
  (log/info "mean-arith")
  (/ (sum datum) (- (count datum) 1)))

(defn mean-geometric
  "compute the geometric mean of the int components of datum"
  [datum]
  (log/info "mean-geometric")
  (/ (mult datum) (- (count datum) 1)))

(defn median
  "compute the media of the int components of datum"
  [datum]
  (let [sorted (sort (filter integer? datum))
        c (count datum)
        midPoint  (int (/ c 2))]
    (if (odd? c)
      (nth sorted midPoint)
      (/ (+ (nth sorted midPoint)
            (nth sorted  (dec midPoint)))
         2))))

(defn mode
  "compute the mode of the int components of datum"
  [datum]
  (log/info "mode")
  (apply max (filter integer? datum)))

