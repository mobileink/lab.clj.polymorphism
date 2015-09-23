(ns dependent-typing.foo-map.proto.api
  (:refer-clojure :exclude [count])
  (:require [clojure.tools.logging :as log :only [debug info]]))

(clojure.core/println "loading dependent-typing.foo-map.proto.api")

(defprotocol PFoo
  "protocol for Foo datatype"
  (foo? [datum])
  (count [datum])
  (sum [datum])
  (mult [datum])
  (mean [datum])
  (mean-geometric [datum])
  (median [datum])
  (mode [datum])
  )
