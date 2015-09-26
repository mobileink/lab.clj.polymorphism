(ns rsch.sig
  (:require [clojure.tools.logging :as log :only [debug info]]))

  (import '(java.lang.reflect Modifier Constructor)
          '(clojure.asm ClassWriter ClassVisitor Opcodes Type)
          '(clojure.asm.commons Method GeneratorAdapter)
          '(clojure.lang IPersistentMap))

(clojure.core/println "loading ns" *ns*)

;; (defn- is-annotation? [c]
;;   (and (class? c)
;;        (.isAssignableFrom java.lang.annotation.Annotation c)))

;; (defn- is-runtime-annotation? [^Class c]
;;   (boolean 
;;    (and (is-annotation? c)
;;         (when-let [^java.lang.annotation.Retention r 
;;                    (.getAnnotation c java.lang.annotation.Retention)] 
;;           (= (.value r) java.lang.annotation.RetentionPolicy/RUNTIME)))))

;; (defn- descriptor [^Class c] (clojure.asm.Type/getDescriptor c))

;; (declare process-annotation)
;; (defn- add-annotation [^clojure.asm.AnnotationVisitor av name v]
;;   (cond
;;    (vector? v) (let [avec (.visitArray av name)]
;;                  (doseq [vval v]
;;                    (add-annotation avec "value" vval))
;;                  (.visitEnd avec))
;;    (symbol? v) (let [ev (eval v)]
;;                  (cond 
;;                   (instance? java.lang.Enum ev)
;;                   (.visitEnum av name (descriptor (class ev)) (str ev))
;;                   (class? ev) (.visit av name (clojure.asm.Type/getType ev))
;;                   :else (throw (IllegalArgumentException. 
;;                                 (str "Unsupported annotation value: " v " of class " (class ev))))))
;;    (seq? v) (let [[nested nv] v
;;                   c (resolve nested)
;;                   nav (.visitAnnotation av name (descriptor c))]
;;               (process-annotation nav nv)
;;               (.visitEnd nav))
;;    :else (.visit av name v)))

;; (defn- process-annotation [av v]
;;   (if (map? v) 
;;     (doseq [[k v] v]
;;       (add-annotation av (name k) v))
;;     (add-annotation av "value" v)))

;; (defn- add-annotations
;;   ([visitor m] (add-annotations visitor m nil))
;;   ([visitor m i]
;;      (doseq [[k v] m]
;;        (when (symbol? k)
;;          (when-let [c (resolve k)]
;;            (when (is-annotation? c)
;;                                         ;this is known duck/reflective as no common base of ASM Visitors
;;              (let [av (if i
;;                         (.visitParameterAnnotation visitor i (descriptor c) 
;;                                                    (is-runtime-annotation? c))
;;                         (.visitAnnotation visitor (descriptor c) 
;;                                           (is-runtime-annotation? c)))]
;;                (process-annotation av v)
;;                (.visitEnd av))))))))


;; (def ^{:private true} prim->class
;;      {'int Integer/TYPE
;;       'ints (Class/forName "[I")
;;       'long Long/TYPE
;;       'longs (Class/forName "[J")
;;       'float Float/TYPE
;;       'floats (Class/forName "[F")
;;       'double Double/TYPE
;;       'doubles (Class/forName "[D")
;;       'void Void/TYPE
;;       'short Short/TYPE
;;       'shorts (Class/forName "[S")
;;       'boolean Boolean/TYPE
;;       'booleans (Class/forName "[Z")
;;       'byte Byte/TYPE
;;       'bytes (Class/forName "[B")
;;       'char Character/TYPE
;;       'chars (Class/forName "[C")})

;; (defn- ^Class the-class [x]
;;   (cond
;;    (class? x) x
;;    (contains? prim->class x) (prim->class x)
;;    :else (let [strx (str x)]
;;            (clojure.lang.RT/classForName
;;             (if (some #{\. \[} strx)
;;               strx
;;               (str "java.lang." strx))))))

;; (defn- ^Type asm-type
;;   "Returns an asm Type object for c, which may be a primitive class
;;   (such as Integer/TYPE), any other class (such as Double), or a
;;   fully-qualified class name given as a string or symbol
;;   (such as 'java.lang.String)"
;;   [c]
;;   (if (or (instance? Class c) (prim->class c))
;;     (Type/getType (the-class c))
;;     (let [strx (str c)]
;;       (Type/getObjectType
;;        (.replace (if (some #{\. \[} strx)
;;                    strx
;;                    (str "java.lang." strx))
;;                  "." "/")))))

;; (defn- generate-interface
;;   [{:keys [name extends methods]}]
;;   (when (some #(-> % first clojure.core/name (.contains "-")) methods)
;;     (throw
;;       (IllegalArgumentException. "Interface methods must not contain '-'")))
;;   (let [iname (.replace (str name) "." "/")
;;         cv (ClassWriter. ClassWriter/COMPUTE_MAXS)]
;;     (. cv visit Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC
;;                                 Opcodes/ACC_ABSTRACT
;;                                 Opcodes/ACC_INTERFACE)
;;        iname nil "java/lang/Object"
;;        (when (seq extends)
;;          (into-array (map #(.getInternalName (asm-type %)) extends))))
;;     (when (not= "NO_SOURCE_FILE" *source-path*) (. cv visitSource *source-path* nil))
;;     (add-annotations cv (meta name))
;;     (doseq [[mname pclasses rclass pmetas] methods]
;;       (let [mv (. cv visitMethod (+ Opcodes/ACC_PUBLIC Opcodes/ACC_ABSTRACT)
;;                   (str mname)
;;                   (Type/getMethodDescriptor (asm-type rclass)
;;                                             (if pclasses
;;                                               (into-array Type (map asm-type pclasses))
;;                                               (make-array Type 0)))
;;                   nil nil)]
;;         (add-annotations mv (meta mname))
;;         (dotimes [i (count pmetas)]
;;           (add-annotations mv (nth pmetas i) i))
;;         (. mv visitEnd)))
;;     (. cv visitEnd)
;;     [iname (. cv toByteArray)]))

;; (import '(clojure.lang DynamicClassLoader))

;; (defmacro gen-interface
;;   [& options]
;;   (log/debug "gen-interface" options)
;;     (let [options-map (apply hash-map options)
;;           [cname bytecode] (generate-interface options-map)]
;;       (log/debug "cname" cname)
;;       (log/debug "bytecode" bytecode)
;;       (when *compile-files*
;;         (clojure.lang.Compiler/writeClassFile cname bytecode))
;;       (let [dc (.defineClass ^DynamicClassLoader (deref clojure.lang.Compiler/LOADER)
;;                     (str (:name options-map)) bytecode options)]
;; (log/debug "dc" dc (type dc))
;;         dc)))

;; (defn- assert-same-protocol [protocol-var method-syms]
;;   (doseq [m method-syms]
;;     (let [v (resolve m)
;;           p (:protocol (meta v))]
;;       (when (and v (bound? v) (not= protocol-var p))
;;         (binding [*out* *err*]
;;           (println "Warning: protocol" protocol-var "is overwriting"
;;                    (if p
;;                      (str "method " (.sym v) " of protocol " (.sym p))
;;                      (str "function " (.sym v)))))))))

(defn- emit-method-builder [on-interface method on-method arglists]
  (let [methodk (keyword method)
        gthis (with-meta (gensym) {:tag 'clojure.lang.AFunction})
        ginterf (gensym)]
    (log/debug "on-interface" on-interface)
    (log/debug "on-method" on-method)
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

(defn- parse-sigparts
  [s]
  ;; (log/debug "parsing: " (type s) s )
  (loop [opts {} [k v & rs :as s] s]
    (if (keyword? k)
      (recur (assoc opts k v) rs)
      [opts s])))


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
  (log/debug "opts+sigs" opts+sigs)
  (let [[opts sigs] (if (string? (first opts+sigs))
                      [{:doc (first opts+sigs)} (next opts+sigs)]
                      [{} opts+sigs])
        ;; log (log/debug "OPTS" opts)
        ;; log (log/debug "SIGS" sigs)
        [opts sigs reducts] (if-let [reducts (if (= (first sigs) :expand) (second sigs) nil)]
                             [opts (next (next sigs)) reducts]
                             [opts sigs nil])
        ;; log (log/debug "opts" opts)
        ;; log (log/debug "sigs" sigs)
        log (log/debug "reducts" reducts)

        [opts sigs]
        (loop [opts {} sigs sigs]
          (condp #(%1 %2) (first sigs)
            ;; string? (recur (assoc opts :doc (first sigs)) (next sigs))
            keyword? (recur (assoc opts (first sigs) (second sigs)) (nnext sigs))
            (do #_(log/debug "last sigs:" sigs)
                #_(log/debug "last opts:" opts)
            [opts sigs])))
        model-ns (:ns opts)

        iname (symbol (str (munge (namespace-munge model-ns)) "." (munge signame)))
        opts (into opts {:on (list 'quote iname) :on-interface iname})

        universe (:universe opts)
        ;; ops (:operators opts)
        laws (:law opts)
        usym (:sym universe)
        urestriction (:restriction universe)
        constants (into #{}
                        (concat (:constants (first sigs))
                                (when reducts (:constants (eval (first reducts))))))
log (log/debug "OUT2" (:constants (eval (first reducts))))

        sigs (:operators (first sigs))
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
        ;; log (log/debug "meths" meths)
        svar (gensym)
        ]
  `(do
     ;; (log/debug "~out:" '~meths)
     (let [ns# (create-ns '~model-ns)
           ~svar (intern ns# '~signame {})
           U# (if (nil? ~universe) (symbol #_(str ns#) "U") ~universe)]
       (gen-interface :name ~iname :methods ~meths)
       ;;     (alter-meta! ~svar #_(var ~signame) assoc :doc ~(:doc opts))
       ;; ~(when sigs
       ;;    `(#'assert-same-protocol ~svar #_(var ~signame) '~(map :name (vals sigs))))
       (alter-var-root ~svar merge
                     (assoc '~opts
                            :constants '~constants
                       :sigs '~sigs
                       :var ~svar
                       :reducts ~reducts
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
     (-reset-methods ~svar) ;; ~signame)
     ~svar))))


    ;; (log/debug "model-ns" '~model-ns))))

(defmacro declare-signature!
  [name & opts+sigs]
  (emit-signature name opts+sigs))

;; (defmacro signature!
;;   [signame & args]
;;   (let [{model-ns :ns
;;          universe :universe
;;          constants :constants
;;          ops :operators
;;          laws :laws} (first (parse-sigparts args))
;;         usym (:sym universe)
;;         urestriction (:restriction universe)
;; log (log/debug "u:" universe)
;;         sigs nil
;;         meths nil
;;         opts nil
;;         ]
;;     `(do
;;        (let [ns# (create-ns '~model-ns)
;;              svar# (intern ns# '~signame {})
;;              U# (if (nil? ~universe) (symbol #_(str ns#) "U") ~universe)]
;;          (log/debug "urestriction: " '~urestriction)
;;          ;;     (gen-interface :name ~iname :methods ~meths)
;;          ;;     (alter-meta! (var ~signame) assoc :doc ~(:doc opts))
;;          ;; ~(when sigs
;;          ;;    `(#'assert-same-protocol (var ~signame) '~(map :name (vals sigs))))
;;          (alter-var-root svar# merge ;; (var ~signame) merge
;;                          (assoc ~opts
;;                                 :sigs '~sigs
;;                                 :var svar# ;; (var ~signame)
;;                                 :universe {:sym ~usym :restriction '~urestriction}
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
;;                                                             (merge '~s {:protocol svar# #_(var ~signame)})))
;;                                             (emit-method-builder (:on-interface opts) (:name s) (:on s) (:arglists s))])
;;                                          (vals sigs)))))
;;          (-reset-methods svar#) ; ~signame)
;;          svar#))))

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


;; core_deftype.clj
;; (defn -reset-methods [protocol]
;;   (doseq [[^clojure.lang.Var v build] (:method-builders protocol)]
;;     (let [cache (clojure.lang.MethodImplCache. protocol (keyword (.sym v)))]
;;       (.bindRoot v (build cache)))))

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

;; (model-signature S
;;   :name M
;;   :ns foo.bar
;;   :dom java.lang.Long ; map semantic type to implementation type
;;   :constants {:a 0 :b 1}
;;   :operators {:f1 foo/f1              ; op semantics
;;               :f2 bar/f2})

(defmacro model-signature!
  [sig & args]
  (let [{model-name :name
         model-ns :ns
         constants :constants
         ops :operators} (first (parse-sigparts args))
        sigs nil
        meths nil
        opts nil
        ;; mvar nil
        ]
    `(do
       ;; (log/debug (str "mvar name: " '~model-name))
       (let [ns# (create-ns '~model-ns)
             mvar# (intern ns# '~model-name {})]
         (log/debug "mvar: " mvar# (type mvar#))
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
