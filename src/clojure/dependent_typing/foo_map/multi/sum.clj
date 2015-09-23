(in-ns 'dependent-typing.foo-map.multi.core)

(clojure.core/println "loading dependent-typing.foo-map.multi.sum")

(defn l-or-v-sum
  [datum]
  (log/info "l-or-v-sum")
  (some? (some #{:foo} datum)))

(defn m-or-s-sum
  [datum]
  (log/info "m-or-s-sum")
  (contains? datum :foo))

(defmulti sum class)

(defmethod sum  clojure.lang.IPersistentList
  [datum]
  (log/info "IPersistentList.sum")
  (l-or-v-sum datum))

(defmethod sum  clojure.lang.IPersistentMap
  [datum]
  (log/info "IPersistentMap.sum")
  (m-or-s-sum datum))

(defmethod sum  clojure.lang.IPersistentSet
  [datum]
  (log/info "IPersistentSet.sum")
  (l-or-v-sum datum))

(defmethod sum  clojure.lang.IPersistentVector
  [datum]
  (log/info "IPersistentVector.sum")
  (l-or-v-sum datum))
