(ns algebra.signature.magma
  (:require [clojure.tools.logging :as log :only [debug info]]))

(clojure.core/println "loading algebra.signature.magma")

(defn mag [a] (log/debug "mag"))
(def magma true)
(defn nbr-mult [t a b] (* a b))
(defn str-concat [a b] (str a b))


(defprotocol Operators
  "Operator Signature for Magmas"
  (** [a b] [t a b]))

(deftype Magma [])

(extend Magma
  Operators
  {:** nbr-mult})

(def M (Magma.))
(extend java.lang.Long
  Operators
  {:** (fn [a b] (** M a b))})


(defn- parse-sigparts
  [s]
  (log/debug "parsing: " (type s) s )
  (loop [opts {} [k v & rs :as s] s]
    (if (keyword? k)
      (recur (assoc opts k v) rs)
      [opts s])))

(defmacro defsignature
  [& args]
  (let [{name :name
         cs :constants
         ops :operators
         laws :laws} (first (parse-sigparts args))]
    ;;  [[name :name, constants :constants operators :operators laws :laws]]
    (log/debug "name:" name)
    (log/debug "constants" cs)
    (log/debug "ops" ops)
    (log/debug "laws" laws)))

;; from core_deftyp:
;; [name fields & opts+specs]
;; (let [gname name
;;       [interfaces methods opts] (parse-opts+specs opts+specs)
;;       ns-part (namespace-munge *ns*)
;;       classname (symbol (str ns-part "." gname))
;;       hinted-fields fields
;;       fields (vec (map #(with-meta % nil) fields))
;;       [field-args over] (split-at 20 fields)]
;;   `(let []
;;      ~(emit-deftype* name gname (vec hinted-fields) (vec interfaces) methods opts)
;;      (import ~classname)
;;      ~(build-positional-factory gname classname fields)
;;      ~classname)))

(defsignature
  :name "mysig"
  :constants {:a 0 :b 1 :c 2}
  :operators {:f1 0 :f2 2}
  :laws "some equations here")
