(ns algebra.signature.group
  (:refer-clojure :exclude [name])
  (:require [potemkin :as pot]
            [clojure.tools.logging :as log :only [debug info]]))

(clojure.core/println "loading algebra.signature.group")

(pot/import-vars [algebra.signature.magma])

;; constants
(defonce id (atom 0))

;; operators
(defprotocol Operators
  "Operator Signature for Groups"
  (commutativity [a] [t a]))
  ;; (** [arg1 arg2] [t arg1 arg2])
  ;; (constants [t]))

;; laws
(defn closure [arg1 arg2])
(defn associativity [arg1 arg2])
(defn inverse [arg1 arg2])

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;  installed structures - namespaces containing implementations of the signatures
;; (def installed-structs
;;   (array-map
;;    :default 'algebra.structure.group.default
;;    :n0 'algebra.structure.group.n0
;;    :n1 'algebra.structure.group.n1
;;    :quotient-3 'algebra.structure.group.q3
;;    ))

;; ;; active model keyword
;; (def ^:dynamic *active-model* :default)

;; ;; map struct keys to struct objects
;; (defonce model-lookup (atom {}))

;; (defn get-struct-kw
;;   "Returns keyword for struct"
;;   [struct]
;;   ;; (log/debug "get-struct-kw: " struct)
;;   (if (keyword? struct)
;;     struct
;;     (structure struct)))

;; (defn install
;;   "Registers a structure for use in a Group model."
;;   ([struct]
;;    (do
;;      ;; (log/debug "install 1: " struct (type struct))
;;      (let [kw (struct-kw struct)]
;;        ;; (log/debug "install 1 kw: " kw)
;;        (install kw struct))))
;;   ([key struct]
;;    (do
;;      ;; (log/debug "install 2: " key struct)
;;      (if (keyword? key)
;;        (do
;;          ;; (log/info (str "model-lookup: " @model-lookup))
;;          ;; (log/info (str "registering " (name struct), ", model type: " (class struct)))
;;          (swap! model-lookup assoc key struct)
;;          ;; (log/info (str "model-lookup: " @model-lookup))
;;          )
;;        (throw (RuntimeException. "arg1 must be clojure keyword"))))))

;; (defn- try-load-model
;;   ([k]
;;    ;; (log/debug "try-load-model: " k)
;;    (if-let [model (@model-lookup k)]
;;      (do
;;           ;; (log/debug "model: " model)
;;           model)
;;      (if-let [ns-sym (installed-structs k)]
;;        (do
;;          ;; (log/debug "ns-sym: " ns-sym)
;;          (try
;;            (do
;;              (require ns-sym)
;;              (@model-lookup k))
;;            (catch Throwable t nil)))
;;        ;; (log/debug "struct " k "not found")
;;        ))))

;; (defn dispatch-type
;;   "Gets the struct object for a keyword or structure.  The struct object
;;   will be used to parameterize Group operations by struct type; in
;;   effect this is a way of dynamically selecting a model to determine
;;   the interpretion of an operation.

;;   Arg is keyword-or-structure."
;;   ([]
;;    (do
;;      ;; (log/debug "dispatch-type 0")
;;      (dispatch-type *active-model*)))
;;   ([kors]
;;    (do
;;      ;; (log/debug "dispatch-type 1" kors)
;;     (let [k (get-struct-kw kors)
;;           ;; log (log/debug "struct k: " k)
;;           obj (@model-lookup k)
;;           ;; log (log/debug "struct: " obj)
;;           ]
;;       (if k
;;         (or obj
;;            (if (try-load-model k) (@model-lookup k))
;;            ;; Why? (when-not (keyword? m) m)
;;            nil)
;;         nil)))))

;; (defn activate
;;   "Activates model, which determines interpretation of Group ops.  Arg is key-or-structure."
;;   ([kors]
;;    ;; (log/debug "activating model: " kors)
;;     (when (keyword? kors)
;;       (let [m (try-load-model kors)
;;             kw (get-struct-kw kors)
;;             c (constants m)]
;;         ;; (log/debug "loaded model: " m)
;;         ;; (log/debug "constants: " c)
;;         (alter-var-root (var *active-model*)
;;                         (fn [_] kw))
;;         ;; (log/debug "*active-model*: " *active-model*)
;;         ;; (log/debug "id: " @id " to " (:id c))
;;         (swap! id (fn [_] (:id c)))
;;         ;; (alter-var-root (var *id*)
;;         ;;                 (fn [_] e))
;;         ;; (log/debug "@id: " @id)
;;         ))))
