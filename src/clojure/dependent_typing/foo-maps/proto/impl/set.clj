(ns dependent-types.foo.proto.impl.set
  (:require [clojure.tools.logging :as log :only [debug info]]))

(defn median
  "compute the media of the int components of datum"
  [datum]
  (log/debug "median")
  )

(defn mode
  "compute the mode of the int components of datum"
  [datum]
  (log/debug "mode")
  )

