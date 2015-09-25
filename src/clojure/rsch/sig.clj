(ns rsch.sig
  (:require [clojure.tools.logging :as log :only [debug info]]))

(clojure.core/println "loading rsch.defsig")

(defn- parse-sigparts
  [s]
  ;; (log/debug "parsing: " (type s) s )
  (loop [opts {} [k v & rs :as s] s]
    (if (keyword? k)
      (recur (assoc opts k v) rs)
      [opts s])))


;; TODO:  support :import, to incorporate other signatures
;;  e.g. for Monoid, :import Magma; for Group, :import Monoid, etc.

(defmacro declare-signature
  [name & args]
  (let [{ns :ns
         cs :constants
         ops :operators
         laws :laws} (first (parse-sigparts args))]
    ;;  [[name :name, constants :constants operators :operators laws :laws]]
    ;; (log/debug "name:" name)
    ;; (log/debug "ns:" ns)
    ;; (log/debug "constants" cs)
    ;; (log/debug "ops" ops)
    ;; (log/debug "laws" laws)
    ))

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


;; (model-signature S
;;   :name M
;;   :ns foo.bar
;;   :constants {:a 0 :b 1}
;;   :operators {:f1 foo/f1              ; op semantics
;;               :f2 bar/f2})

;; core_deftype.clj
;; (defn -reset-methods [protocol]
;;   (doseq [[^clojure.lang.Var v build] (:method-builders protocol)]
;;     (let [cache (clojure.lang.MethodImplCache. protocol (keyword (.sym v)))]
;;       (.bindRoot v (build cache)))))

(defn- emit-method-builder [on-interface method on-method arglists]
  )

;; TODO: rules.  Only one sig per model; only one underlying type?

;; Problem: what about vector spaces, rings, fields.  rings and fields
;; can be handled by sig decl.  Vector spaces require two semantic
;; types, e.g. real scalars and vector of reals.  The vectors could be
;; modeled by lists, vectors, or some other implementation.

;; in general, we want to keep semantic type and implementation type
;; distinct.  e.g. semantic type might be sequence; implementation
;; could be vector, list, etc.  so semantic type goes in signature?
;; that makes sense, since sig symbols partitioned on presumption that
;; semantic domain is partitioned, e.g. val constant symbols
;; v. function symbols.  So concept of underlying type is present,
;; abstractly, in the signature.  Usually this is meta - the sig does
;; not include a symbol for "underlying type."  For our purposes we
;; can allow the user to specify the (meta) symbol for the types in a
;; sig, default to T.  Then the model assigns an implementation type
;; to T.  Then we can use symbol 'T' to express e.g. law of closure:
;; a*b \in T.

(defmacro model-signature
  [sig & args]
  (let [{model-name :name
         model-ns :ns
         constants :constants
         ops :operators} (first (parse-sigparts args))
        sigs nil
        meths nil
        opts nil
        old-ns *ns*
        new-ns (create-ns (symbol (str model-ns)))
        ;; mvar nil
        ]
    (log/debug "model-name: " model-name (type model-name))
    (log/debug "old ns: " old-ns)
    (log/debug "new ns: " new-ns (type new-ns))
    `(do
       (log/debug (str "mvar name: " '~model-name))
       (let [mvar# (intern ~new-ns '~model-name {})]
         (log/debug "mvar: " mvar#)
         ;;     (gen-interface :name ~iname :methods ~meths)
         ;;     (alter-meta! (var ~model-name) assoc :doc ~(:doc opts))
         ;; ~(when sigs
         ;;    `(#'assert-same-protocol (var ~model-name) '~(map :name (vals sigs))))
         (alter-var-root mvar# merge ;; (var ~model-name) merge
                         (assoc ~opts
                                :sigs '~sigs
                                :var mvar# ;; (var ~model-name)
                                :consts ~constants
                                :method-map
                                ~(and (:on opts)
                                      (apply hash-map
                                             (mapcat
                                              (fn [s]
                                                [(keyword (:name s)) (keyword (or (:on s) (:name s)))])
                                              (vals sigs))))
                                :method-builders
                                ~(apply hash-map
                                        (mapcat
                                         (fn [s]
                                           [`(intern *ns* (with-meta '~(:name s)
                                                            (merge '~s {:protocol mvar# #_(var ~model-name)})))
                                            (emit-method-builder (:on-interface opts) (:name s) (:on s) (:arglists s))])
                                         (vals sigs)))))
         (-reset-methods mvar#) ; ~model-name)
         ;; (in-ns (symbol (str ~old-ns)))
         ;; '~model-name
         mvar#))))

(defmacro with-model
  "install a model and execute body in that environment"
  ;; find the model (find-ns?), pull the constants and ops, and `let`
  ;; them, establishing the local environment and shadowing any
  ;; enclosing env.  then execute body.
  [sig & body]
  (log/debug "sig" sig (type sig))
  ;; FIXME: sig must by strongly typed?
  (let [sig-ns (find-ns (symbol (namespace sig)))
        consts (:consts (eval sig)) ;; FIXME: do this within body?
        log (log/debug "consts" consts)
        bindv (into [] (flatten (for [[k v] consts] [(symbol (name k)) v])))
        args (seq bindv)]
    (log/debug "sig:" sig (type sig))
    (log/debug "sig-ns:" sig-ns)
    (log/debug "binv" bindv)
    (log/debug "args" args)
    `(println [~@bindv])
    `(let [~@bindv]
       ~(first body)
       )
    ))

(declare-signature MySig
  :ns foo.bar
  :constants #{:a :b :c}
  :operators #{(f1 [a]) (f2 [a b])}
  :laws #{(closure [a b] ...impl...)
          (associativity [a b c] ...impl...)})

;; :constants - this list will tell model-signature what vars to
;; intern in the model ns.  in this example, a, b, and c become
;; available as vars within the model ns.

;; :ops is a list of fn declarations;

;; :laws a list of fn definitions a law should be a
;; proposition (equality or inequality), although we
;; have no way to enforce that.  so they serve two purposes:
;; documentation, they express the laws of the algebra, and testing


;; on namespaces - we can intern vars, and use this to represent
;; constants within a model ns.  but what about client namespaces?  we
;; need to intern our constants in them, but that would risk name clashes
;; - don't want to overwrite a name the client has already interned.  so
;; our constants must be namespaced, just like our funcs.  we will not be
;; able to write simply (foo/** 3 e) but will have to write (foo/** 3 foo/e)

;; an alternative would be to use a macro with-signature or with-algebra
;; in order to set up a local namespace.  then we could write:

;; (with-model Foo
;;   (** 3 e)
;;   ...)
