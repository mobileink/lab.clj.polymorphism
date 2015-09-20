(ns algebra.signature.group
  (:refer-clojure :exclude [name])
  (:require ;algebra.models.group
            [clojure.tools.logging :as log :only [debug info]]))

(clojure.core/println "loading algebra.signature.group")

(def ^:dynamic *id* nil)

(defn reset-id [n]
  (alter-var-root (var *id*)
                  (fn [_] n)))

(defprotocol OpSigGroup
  "Operator Signature for Groups"
  (name [t])
  (^Keyword struct-kw [t]) ;; every implementation must provide a keyword struct-type tag
  (** [arg1 arg2] [t arg1 arg2])
  (id [t])
  (id? [t] [t arg]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def KNOWN-STRUCTS
  (array-map
   :default 'java.lang.Object
   :n0 'algebra.struct.N0
   :n1 'algebra.struct.n1
   :quotient-3 'algebra.struct.q3
   ))

;; current model keyword
(def ^:dynamic *current-model* :default)

;; map struct keys to struct objects
(defonce model-lookup (atom {}))

(defn get-struct-kw
  "Returns keyword for struct"
  [struct]
  ;; (log/debug "get-struct-kw: " struct)
  (if (keyword? struct)
    struct
    (struct-kw struct)))

(defn register-struct
  "Registers a structure for use in a Group model."
  ([struct]
   (do
     ;; (log/debug "register-struct 1: " struct (type struct))
     (let [kw (struct-kw struct)]
       ;; (log/debug "register-struct 1 kw: " kw)
       (register-struct kw struct))))
  ([key struct]
   (do
     ;; (log/debug "register-struct 2: " key struct)
     (if (keyword? key)
       (do
         ;; (log/info (str "model-lookup: " @model-lookup))
         ;; (log/info (str "registering " (name struct), ", model type: " (class struct)))
         (swap! model-lookup assoc key struct)
         ;; (log/info (str "model-lookup: " @model-lookup))
         )
       (throw (RuntimeException. "arg1 must be clojure keyword"))))))

(defn- try-load-model
  ([k]
   ;; (log/debug "try-load-model: " k)
   (if-let [model (@model-lookup k)]
     (do
          ;; (log/debug "model: " model)
          model)
     (if-let [ns-sym (KNOWN-STRUCTS k)]
       (do
         ;; (log/debug "ns-sym: " ns-sym)
         (try
           (do
             (require ns-sym)
             (@model-lookup k))
           (catch Throwable t nil)))
       ;; (log/debug "struct " k "not found")
       ))))

(defn dispatch-param
  "Gets the struct object for a keyword or struct.  The struct object
  will be used to parameterize Group operations by struct type; in
  effect this is a way of dynamically selecting a model to determine
  the interpretion of an operation.

  Arg is keyword-or-struct."
  ([]
   (do
     ;; (log/debug "dispatch-param 0")
     (dispatch-param *current-model*)))
  ([kors]
   (do
     ;; (log/debug "dispatch-param 1" kors)
    (let [k (get-struct-kw kors)
          ;; log (log/debug "struct k: " k)
          obj (@model-lookup k)
          ;; log (log/debug "struct: " obj)
          ]
      (if k
        (or obj
           (if (try-load-model k) (@model-lookup k))
           ;; Why? (when-not (keyword? m) m)
           nil)
        nil)))))

(defn set-model
  "Set the current model, which determines interpretation of Group ops.  Arg is key-or-struct."
  ([kors]
   ;; (log/debug "set-model: " kors)
    (when (keyword? kors)
      (let [m (try-load-model kors)
            kw (get-struct-kw kors)
            e (id m)]
        ;; (log/debug "loaded model: " m)
        (alter-var-root (var *current-model*)
                        (fn [_] kw))
        ;; (log/debug "*current-model*: " *current-model*)
        ;; (log/debug "*id*: " *id* " to " e)
        (alter-var-root (var *id*)
                        (fn [_] e))
        ;; (log/debug "*id*: " *id*)
        ))))
