(ns dependent-typing.foo-map.proto.core
  (:require [dependent-typing.foo-map.proto.api :as foo]
            [clojure.data.generators :as g]
            [clojure.tools.logging :as log :only [debug info]]))

(clojure.core/println "loading dependent-typing.foo-map.proto.core")

;; create some test data

(def nums (take 10 (distinct (repeatedly #(rand-int 1000)))))
(def ks (map #(keyword (str %))
             (take 10 (distinct (filter #(Character/isLowerCase %)
                                        (repeatedly #(g/printable-ascii-char)))))))
(def mfoo (into {:foo true} (zipmap ks nums)))
(def sfoo (into #{:foo} nums))
(def lfoo (into '(:foo) nums))
(def vfoo (into [:foo] nums))

(defn l [] lfoo)
(defn m [] mfoo)
(defn s [] sfoo)
(defn v [] vfoo)

(defn myfun []
  ;; do some foo computation
  (let [lmean (foo/mean lfoo)
        mmean (foo/mean mfoo)]
    (println "foo data with means:")
    (println "foo-list: " lfoo " => " lmean)
    (println "foo-map:  " mfoo " => " mmean)
    (println "foo-set:  " sfoo " => " (foo/mean sfoo))
    (println "foo-vector:  " vfoo " => " (foo/mean vfoo))))


;; now in the repl use the api:

;; (require '(foo [core :as x]))
;; user=> (foo/mean (x/l))
;; user=> (foo/mean (x/m))

