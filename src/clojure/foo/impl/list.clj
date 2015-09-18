(ns foo.impl.list
  (:require [clojure.tools.logging :as log :only [debug info]]))

(defn median
  "compute the media of the int components of datum"
  [datum]
  (log/info "median")
  )

(defn mode
  "compute the mode of the int components of datum"
  [datum]
  (log/info "mode")
  )

