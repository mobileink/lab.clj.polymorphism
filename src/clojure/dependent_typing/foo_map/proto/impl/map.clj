(ns dependent-typing.foo-map.proto.impl.map
  (:refer-clojure :exclude [count])
  (:require [clojure.tools.logging :as log :only [debug info]]))

(println "loading dependent-typing.foo-map.proto.impl.map")

(defn count
  "count the int components of datum"
  [datum]
  (log/info "count")
  (let [is (vals datum)]
    (clojure.core/count (filter integer? is))))

(defn sum
  "sum the int components of datum"
  [datum]
  (log/info "sum")
  (reduce + (filter integer? (vals datum))))

(defn mult
  "multiply the int components of datum"
  [datum]
  (log/info "mult")
  (reduce * (filter integer? (vals datum))))

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
  (log/info "median")
  (let [sorted (sort (filter integer? (vals datum)))
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
  (apply max (filter integer? (vals datum))))

