(ns dependent-types.foo.proto.impl.foo
  (:refer-clojure :exclude [count])
  (:require [dependent-types.foo.proto.api :as api]
            [dependent-types.foo.proto.impl.list :as foo-list]
            [dependent-types.foo.proto.impl.map :as foo-map]
            [dependent-types.foo.proto.impl.set :as foo-set]
            [dependent-types.foo.proto.impl.vector :as foo-vector]
            [dependent-types.foo.proto.impl.lsv :as foo-lsv]
            [clojure.tools.logging :as log :only [debug info]]
            ))

(println "loading dependent-types.foo.proto.impl.foo")

(extend clojure.lang.IPersistentList
  api/PFoo
  {:foo? foo-lsv/l-or-v-foo?
   :count foo-lsv/count
   :sum foo-lsv/sum
   :mult foo-lsv/mult
   :mean foo-lsv/mean-arith
   :mean-geometric foo-lsv/mean-geometric
   :median foo-lsv/median
   :mode foo-lsv/mode})

(extend clojure.lang.IPersistentMap
  api/PFoo
  {:foo? foo-lsv/m-or-s-foo?
   :count foo-map/count
   :sum foo-map/sum
   :mult foo-map/mult
   :mean foo-map/mean-arith
   :mean-geometric foo-map/mean-geometric
   :median foo-map/median
   :mode foo-map/mode})

(extend clojure.lang.IPersistentSet
  api/PFoo
  {:foo? foo-lsv/m-or-s-foo?
   :count foo-lsv/count
   :sum foo-lsv/sum
   :mult foo-lsv/mult
   :mean foo-lsv/mean-arith
   :mean-geometric foo-lsv/mean-geometric
   :median foo-lsv/median
   :mode foo-lsv/mode})

(extend clojure.lang.IPersistentVector
  api/PFoo
  {:foo? foo-lsv/l-or-v-foo?
   :count foo-lsv/count
   :sum foo-lsv/sum
   :mult foo-lsv/mult
   :mean foo-lsv/mean-arith
   :mean-geometric foo-lsv/mean-geometric
   :median foo-lsv/median
   :mode foo-lsv/mode})


