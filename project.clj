(defproject org.mobileink/lab.protocols "0.3.3-SNAPSHOT"
  :description "lab.protocols - lab for experimentation with clojure protocols"
  :url "https://github.com/mobileink/lab.clj.protocols"
  :min-lein-version "2.0.0"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :scm {:name "git" :url "https://github.com/mobileink/lab.clj.protocols"}
  :source-paths ["src/clojure"]
  :test-paths ["test/clojure"]
  :test-selectors {:protocols :protocols
                   :multimethods :multimethods}
  :dependencies [[org.clojure/clojure "1.8.0-master-SNAPSHOT"]
                 [org.clojure/tools.reader "0.8.16"]
                 [org.clojure/tools.logging "0.3.1"]
                 [org.clojure/data.generators "0.1.2"]
                 [org.slf4j/slf4j-log4j12 "1.7.1"]
                 [log4j "1.2.17" :exclusions [javax.mail/mail
                                              javax.jms/jms
                                              com.sun.jdmk/jmxtools
                                              com.sun.jmx/jmxri]]]
  :repl-options {:port 8080}
  :profiles {:dev {:source-paths ["src/clojure" "dev"]
                   :dependencies [[org.clojure/tools.namespace "0.2.3"]
                                  [org.clojure/java.classpath "0.2.0"]]}})

