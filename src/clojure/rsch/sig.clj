(ns rsch.sig
  (:require [clojure.tools.logging :as log :only [debug info]]))

  (import '(java.lang.reflect Modifier Constructor)
          '(clojure.asm ClassWriter ClassVisitor Opcodes Type)
          '(clojure.asm.commons Method GeneratorAdapter)
          '(clojure.lang IPersistentMap))

(clojure.core/println "loading ns" *ns*)

(defn- emit-method-builder [on-interface method on-method arglists]
  (let [methodk (keyword method)
        gthis (with-meta (gensym) {:tag 'clojure.lang.AFunction})
        ginterf (gensym)]
    `(fn [cache#]
       (let [~ginterf
             (fn
               ~@(map
                  (fn [args]
                    (let [gargs (map #(gensym (str "gf__" % "__")) args)
                          target (first gargs)]
                      `([~@gargs]
                          (. ~(with-meta target {:tag '~on-interface}) (~(or on-method method) ~@(rest gargs))))))
                  arglists))
             ^clojure.lang.AFunction f#
             (fn ~gthis
               ~@(map
                  (fn [args]
                    (let [gargs (map #(gensym (str "gf__" % "__")) args)
                          target (first gargs)]
                      `([~@gargs]
                          (let [cache# (.__methodImplCache ~gthis)
                                f# (.fnFor cache# (clojure.lang.Util/classOf ~target))]
                            (if f#
                              (f# ~@gargs)
                              ((-cache-protocol-fn ~gthis ~target '~on-interface ~ginterf) ~@gargs))))))
                           arglists))
             ]
         (set! (.__methodImplCache f#) cache#)
         f#))))

;; (defn- parse-sigparts
;;   [s]
;;   ;; (log/debug "parsing: " (type s) s )
;;   (loop [opts {} [k v & rs :as s] s]
;;     (if (keyword? k)
;;       (recur (assoc opts k v) rs)
;;       [opts s])))

;; TODO:  support :import, to incorporate other signatures
;;  e.g. for Monoid, :import Magma; for Group, :import Monoid, etc.

    ;; (log/debug "name:" name)
    ;; (log/debug "ns:" ns)
    ;; (log/debug "constants" cs)
    ;; (log/debug "ops" ops)
    ;; (log/debug "laws" laws)

(defn ^:private ^:static
  reduce1
       ([f coll]
             (let [s (seq coll)]
               (if s
         (reduce1 f (first s) (next s))
                 (f))))
       ([f val coll]
          (let [s (seq coll)]
            (if s
              (if (chunked-seq? s)
                (recur f
                       (.reduce (chunk-first s) f val)
                       (chunk-next s))
                (recur f (f val (first s)) (next s)))
         val))))

(defn- emit-signature [signame opts+sigs]
  ;; (log/debug "opts+sigs" opts+sigs)
  ;; FIXME: validate opts, required and optional

  (let [[opts sigs] (if (string? (first opts+sigs))
                      [{:doc (first opts+sigs)} (next opts+sigs)]
                      [{} opts+sigs])
        ;; [opts sigs reducts] (if-let [reducts (if (= (first sigs) :expand) (second sigs) nil)]
        ;;                      [opts (nnext sigs) reducts]
        ;;                      [opts sigs nil])
        [opts sigs reducts ns universe constants operators laws]
        (loop [opts {}, sigs sigs,
               reducts nil, ns nil, universe nil, constants nil, operators nil, laws nil]
          (condp #(%1 %2) (first sigs)
            ;; string? (recur (assoc opts :doc (first sigs)) (next sigs))
            #(= % :expand) (recur opts (nnext sigs)
                                  (second sigs) ns universe constants operators laws)
            #(= % :ns) (recur opts (nnext sigs)
                              reducts (second sigs) universe constants operators laws)
            #(= % :universe) (recur opts (nnext sigs)
                                    reducts ns (second sigs) constants operators laws)
            #(= % :constants) (recur opts (nnext sigs)
                                     reducts ns universe (second sigs) operators laws)
            #(= % :operators) (recur opts (nnext sigs)
                                     reducts ns universe constants (second sigs) laws)
            #(= % :laws) (recur opts (nnext sigs)
                                reducts ns universe constants operators (second sigs))
            keyword? (recur (assoc opts (first sigs) (second sigs)) (nnext sigs)
                            reducts ns universe constants operators laws)
            (do (log/debug "last sigs:" sigs)
                (log/debug "last opts:" opts)
                [opts sigs reducts ns universe constants operators laws])))
                ;; [opts sigs ns universe])))
;; log          (log/debug "opts" opts)
;; log          (log/debug "sigs" sigs)
log          (log/debug "reducts" reducts)
log          (log/debug "ns" ns)
;; log          (log/debug "universe" universe)
;; log          (log/debug "operators" operators)
;; log          (log/debug "constants" constants)
        model-ns ns ; (:ns opts)
        opts (dissoc opts :ns)
        iname (symbol (str (munge (namespace-munge model-ns)) "." (munge signame)))
        opts (into opts {:on (list 'quote iname) :on-interface iname})

        log (log/debug "opts" opts)
        log (log/debug "sigs" sigs)
        log (log/debug "reducts" reducts)

        ;; universe (:universe opts)
        ;; ops (:operators opts)
        ;; laws (:laws opts)
        usym (:sym universe)
        urestriction (:restriction universe)
        ;; constants (:constants (first sigs))
        sigs operators
        sigs (do #_(log/debug "sigs" sigs)
                 (when sigs
               (reduce1 (fn [m s]
                          (let [name-meta (meta (first s))
                                mname (with-meta (first s) nil)
                                [arglists doc]
                                (loop [as [] rs (rest s)]
                                  (if (vector? (first rs))
                                    (recur (conj as (first rs)) (next rs))
                                    [(seq as) (first rs)]))]
                            (when (some #{0} (map count arglists))
                              (throw (IllegalArgumentException. (str "Definition of function " mname " in protocol " signame " must take at least one arg."))))
                            (when (m (keyword mname))
                              (throw (IllegalArgumentException. (str "Function " mname " in protocol " signame " was redefined. Specify all arities in single definition."))))
                            (assoc m (keyword mname)
                                   (merge name-meta
                                          {:name (vary-meta mname assoc :doc doc :arglists arglists)
                                           :arglists arglists
                                           :doc doc}))))
                        {} sigs)))
        meths (mapcat (fn [sig]
                        #_(log/debug "methsig" sig)
                        (let [m (munge (:name sig))]
                          (map #(vector m (vec (repeat (dec (count %))'Object)) 'Object)
                               (:arglists sig))))
                      (vals sigs))
        log (log/debug "meths" meths)
        svar (gensym)
        reducts-map (when reducts @(find-var (first reducts)))
        log (log/debug "reducts-map" reducts-map)
        constants (into #{} (concat constants
                                    (when reducts (:constants reducts-map))))
        log (log/debug "CONSTANTS" constants)
        sigs (into sigs (when reducts  (:sigs reducts-map)))
        ;; log (log/debug "SIGS" sigs)
        laws (into laws (when reducts (:laws reducts-map)))
        ;; log (log/debug "LAWS" laws)
        sname (str model-ns "/" signame)
        log (log/debug "sname" sname)
        ]
  `(do
;     (log/debug (str "ns sname: '" (namespace '~sname) "'"))
;     (log/debug (str "nm sname:" (name '~sname)))
     (let [ns# (create-ns '~model-ns)
           ~svar (intern ns# '~signame {})
           U# (if (nil? ~universe) (symbol #_(str ns#) "U") ~universe)]
       (gen-interface :name ~iname :methods ~meths)
       ;;     (alter-meta! ~svar #_(var ~signame) assoc :doc ~(:doc opts))
       ;; ~(when sigs
       ;;    `(#'assert-same-protocol ~svar #_(var ~signame) '~(map :name (vals sigs))))
       (alter-var-root ~svar merge
                     (assoc '~opts
                            :ns '~model-ns
                            :universe '~universe
                            :constants '~constants
                       :sigs '~sigs
                       :var ~svar
                       ;; :reducts ~reducts
                       :laws '~laws
                       :method-map ~(and #_(:on opts)
                                            (apply hash-map
                                                   (mapcat
                                                    (fn [s]
                                                      [(keyword (:name s))
                                                       (keyword (or (:on s) (:name s)))])
                                                    (vals sigs))))
                       :method-builders ~(apply hash-map
                                                   (mapcat
                                                    (fn [s]
                                                      [`(intern '~model-ns ;; *ns*
                                                                (with-meta '~(:name s)
                                                                  (merge '~s {:protocol ~svar})))
                                                       (emit-method-builder (:on-interface opts)
                                                                            (:name s)
                                                                            (:on s)
                                                                            (:arglists s))])
                                                    (vals sigs)))
                        ))
       (-reset-methods @~svar) ;; ~signame)
       ~svar))))

(defmacro declare-signature!
  [name & opts+sigs]
  (emit-signature name opts+sigs))

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

;; (define-model! M :for S
;;   :dom java.lang.Long ; map semantic type to implementation type
;;   :constants {:a 0 :b 1}
;;   :operators {:f1 foo/f1              ; op semantics
;;               :f2 bar/f2})


(defn- validate-model-constants
  [mc sc] ; model consts (map), and signature consts (set)
  ;; (log/debug "mc" mc)
  ;; (log/debug "sc" sc)
  (if (= (set (keys mc)) sc)
    mc
    (throw (RuntimeException. (str "Model constant defs " mc " do not match sig consts:" sc)))))

(defn- validate-ops
  [ops sigs meth-map meth-bldrs]
  (log/debug "validate-ops")
  (let [bld-keys (doall (map (comp :name meta) (keys meth-bldrs)))]
    (log/debug "bld-keys" bld-keys)
    (log/debug "meth-map" (keys meth-map))
    (log/debug "sigs" sigs)
    (log/debug "ops" ops)
    (if (= (keys ops) (keys meth-map))
      ;; FIXME: check arities
      (log/debug "OK" )
      (throw (RuntimeException. (str "Model op defs" ops " do not match sig decls: " meth-map))))
    ))

(defn- emit-model [signame opts+sigs]
  (let [[opts sigs] (if (string? (first opts+sigs))
                      [{:doc (first opts+sigs)} (next opts+sigs)]
                      [{} opts+sigs])
        [opts sigs ns universe constants operators forsig]
        (loop [opts {}, sigs sigs,
               ns nil, universe nil, constants nil, operators nil, forsig nil]

         ;; (log/debug "opts" opts)
         ;; (log/debug "sigs" sigs)
         ;; (log/debug "forsig" forsig)
         ;; (log/debug "ns" ns)
         ;; (log/debug "universe" universe)
         ;; (log/debug "operators" operators)
         ;; (log/debug "constants" constants)

          (condp #(%1 %2) (first sigs)
            string? (recur (assoc opts :doc (first sigs)) (next sigs)
                           ns universe constants operators forsig)
            ;; #(= % :expand) (recur opts (nnext sigs)
            ;;                       (second sigs) ns universe constants operators forsig)
            #(= % :ns) (recur opts (nnext sigs)
                              (second sigs) universe constants operators forsig)
            #(= % :universe) (recur opts (nnext sigs)
                                    ns (second sigs) constants operators forsig)
            #(= % :constants) (recur opts (nnext sigs)
                                     ns universe (second sigs) operators forsig)
            #(= % :operators) (recur opts (nnext sigs)
                                     ns universe constants (second sigs) forsig)
            #(= % :for) (recur opts (nnext sigs)
                               ns universe constants operators (second sigs))
            keyword? (recur (assoc opts (first sigs) (second sigs)) (nnext sigs)
                            ns universe constants operators forsig)
            (do (log/debug "mdl last sigs:" sigs)
                (log/debug "mdl last opts:" opts)
                [opts sigs ns universe constants operators forsig])))
;; log          (log/debug "opts" opts)
;; log          (log/debug "sigs" sigs)
;; log          (log/debug "forsig" forsig)
;; log          (log/debug "ns" ns)
;; log          (log/debug "universe" universe)
;; log          (log/debug "operators" operators)
;; log          (log/debug "constants" constants)
        ;; model-ns ns ; (:ns opts)
        ;; opts (dissoc opts :ns)
        ;; iname (symbol (str (munge (namespace-munge model-ns)) "." (munge signame)))
        ;; opts (into opts {:on (list 'quote iname) :on-interface iname})

        ;; log (log/debug "opts" opts)
        ;; log (log/debug "sigs" sigs)

        ;; universe (:universe opts)
        ;; ops (:operators opts)
        ;; forsig (:forsig opts)
        usym (:sym universe)
        urestriction (:restriction universe)
        ;; constants (:constants (first sigs))
        ;; sigs operators
        ;; sigs (do #_(log/debug "sigs" sigs)
        ;;          (when sigs
        ;;            (reduce1 (fn [m s]
        ;;                       (let [name-meta (meta (first s))
        ;;                             mname (with-meta (first s) nil)
        ;;                             [arglists doc]
        ;;                             (loop [as [] rs (rest s)]
        ;;                               (if (vector? (first rs))
        ;;                                 (recur (conj as (first rs)) (next rs))
        ;;                                 [(seq as) (first rs)]))]
        ;;                         (when (some #{0} (map count arglists))
        ;;                           (throw (IllegalArgumentException. (str "Definition of function " mname " in protocol " signame " must take at least one arg."))))
        ;;                         (when (m (keyword mname))
        ;;                           (throw (IllegalArgumentException. (str "Function " mname " in protocol " signame " was redefined. Specify all arities in single definition."))))
        ;;                         (assoc m (keyword mname)
        ;;                                (merge name-meta
        ;;                                       {:name (vary-meta mname assoc :doc doc :arglists arglists)
        ;;                                        :arglists arglists
        ;;                                        :doc doc}))))
        ;;                     {} sigs)))
        ;; meths (mapcat (fn [sig]
        ;;                 #_(log/debug "methsig" sig)
        ;;                 (let [m (munge (:name sig))]
        ;;                   (map #(vector m (vec (repeat (dec (count %))'Object)) 'Object)
        ;;                        (:arglists sig))))
        ;;               (vals sigs))
        ;; log (log/debug "meths" meths)
        svar (gensym)
        sigmap @(find-var forsig)
        log (log/debug "sigmap" sigmap)

        constants (validate-model-constants constants (:constants sigmap))
        log (log/debug "constants" constants)

        sigs (:sigs sigmap)
        meth-map  (:method-map sigmap)
        meth-bldrs (:method-builders sigmap)

        ops (validate-ops operators sigs meth-map meth-bldrs)

        ;; for (into forsig (when sigmap (:for sigmap)))
        ;; log (log/debug "FOR" for)
        laws (:laws sigmap)
        log (log/debug "laws" laws)
        ]
  `(do
     (let [ns# (create-ns (symbol (namespace '~signame)))
           ~svar (intern ns# (symbol (name '~signame)) {})]
           ;; U# (if (nil? ~universe) (symbol #_(str ns#) "U") ~universe)]

       ;; (gen-interface :name ~iname :methods ~meths)
       ;;     (alter-meta! ~svar #_(var ~signame) assoc :doc ~(:doc opts))
       ;; ~(when sigs
       ;;    `(#'assert-same-protocol ~svar #_(var ~signame) '~(map :name (vals sigs))))

       (alter-var-root ~svar merge
                     (assoc '~opts
                            :universe '~universe
                            :constants '~constants
                            :operators ~operators
                            :var ~svar
                            ;; :sigmap ~sigmap
                            :signature '~forsig
                            :laws '~laws
                            :method-map '~meth-map
                       ;; :method-map ~(and #_(:on opts)
                       ;;                      (apply hash-map
                       ;;                             (mapcat
                       ;;                              (fn [s]
                       ;;                                [(keyword (:name s))
                       ;;                                 (keyword (or (:on s) (:name s)))])
                       ;;                              (vals sigs))))
                            :method-builders '~meth-bldrs
                       ;; :method-builders ~(apply hash-map
                       ;;                             (mapcat
                       ;;                              (fn [s]
                       ;;                                [`(intern '~model-ns ;; *ns*
                       ;;                                          (with-meta '~(:name s)
                       ;;                                            (merge '~s {:protocol ~svar})))
                       ;;                                 (emit-method-builder (:on-interface opts)
                       ;;                                                      (:name s)
                       ;;                                                      (:on s)
                       ;;                                                      (:arglists s))])
                       ;;                              (vals sigs)))
                        ))
       ;; (-reset-methods @~svar) ;; ~signame)
       '~signame))))

(defmacro define-model!
  [name & opts]
  (emit-model name opts))

;; (defmacro define-model!
;;   [sig & args]
;;   (let [{model-name :name
;;          model-ns :ns
;;          constants :constants
;;          ops :operators} (first (parse-sigparts args))
;;         sigs nil
;;         meths nil
;;         opts nil
;;         ;; mvar nil
;;         ]
;;     `(do
;;        ;; (log/debug (str "mvar name: " '~model-name))
;;        (let [ns# (create-ns '~model-ns)
;;              mvar# (intern ns# '~model-name {})]
;;          (log/debug "mvar: " mvar# (type mvar#))
;;          ;;     (gen-interface :name ~iname :methods ~meths)
;;          ;;     (alter-meta! (var ~model-name) assoc :doc ~(:doc opts))
;;          ;; ~(when sigs
;;          ;;    `(#'assert-same-protocol (var ~model-name) '~(map :name (vals sigs))))
;;          (alter-var-root mvar# merge ;; (var ~model-name) merge
;;                          (assoc ~opts
;;                                 :sigs '~sigs
;;                                 :var mvar# ;; (var ~model-name)
;;                                 :consts ~constants
;;                                 :method-map
;;                                 ~(and (:on opts)
;;                                       (apply hash-map
;;                                              (mapcat
;;                                               (fn [s]
;;                                                 [(keyword (:name s)) (keyword (or (:on s) (:name s)))])
;;                                               (vals sigs))))
;;                                 :method-builders
;;                                 ~(apply hash-map
;;                                         (mapcat
;;                                          (fn [s]
;;                                            [`(intern *ns* (with-meta '~(:name s)
;;                                                             (merge '~s {:protocol mvar# #_(var ~model-name)})))
;;                                             (emit-method-builder (:on-interface opts) (:name s) (:on s) (:arglists s))])
;;                                          (vals sigs)))))
;;          (-reset-methods mvar#) ; ~model-name)
;;          mvar#))))

(defmacro with-model
  "install a model and execute body in that environment"
  ;; find the model (find-ns?), pull the constants and ops, and `let`
  ;; them, establishing the local environment and shadowing any
  ;; enclosing env.  then execute body.
  [model & body]
  (log/debug "model" model (type model))
  ;; FIXME: model must by strongly typed?
  (log/debug "model val" @(find-var model))
  (let [model-ns (find-ns (symbol (namespace model)))
        mdl @(find-var model)
        consts (:constants mdl) ;; FIXME: do this within body?
        log (log/debug "consts" consts)
        bindv (into [] (flatten (for [[k v] consts] [(symbol (name k)) v])))
        args (seq bindv)
        ops (:operators mdl)
        bindops (into bindv
                      (flatten
                       (for [[k v] ops]
                        [(symbol (name k)) v])))]
    (log/debug "model:" model (type model))
    (log/debug "model-ns:" model-ns)
    (log/debug "ops" ops)
    (log/debug "bindv" bindv)
    (log/debug "bindops" bindops)
    (log/debug "args" args)
    `(let [~@bindops]
       ~(first body)
       )
    ))

;; (declare-signature! MySig
;;   :ns foo.bar
;;   :constants #{:a :b :c}
;;   :operators #{(f1 [a]) (f2 [a b])}
;;   :laws #{(closure [a b] ...impl...)
;;           (associativity [a b c] ...impl...)})

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
