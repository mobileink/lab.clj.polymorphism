(println "loading user.clj")

(require '[clojure.test :refer [run-tests test-var]])
(require '[clojure.tools.namespace.repl :refer [refresh refresh-all set-refresh-dirs]])
(require '[clojure.tools.reader.edn :as edn])
(require '[foo.proto.api :as proto-foo])

(set-refresh-dirs "test/clojure" "src/clojure" "src/clojure/foo")

(def m {:foo true :a 1 :b 2 :c 3 :d 4 :e 5})
(def s #{:foo 1 2 3 4 5})
(def l '(:foo 1 2 3 4 5))
(def v [:foo 1 2 3 4 5])

