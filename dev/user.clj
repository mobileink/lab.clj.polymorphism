(println "loading user.clj")

(require '[clojure.test :refer [run-tests test-var]])
(require '[clojure.tools.namespace.repl :refer [refresh refresh-all set-refresh-dirs]])
(require '[clojure.tools.reader.edn :as edn])
(require '[foo.proto.api :as proto-foo])

(set-refresh-dirs "test/clojure" "src/clojure" "src/clojure/foo")


