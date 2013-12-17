(defproject aco-tsp "0.1.0-SNAPSHOT"
  :jvm-opts ["-Xmx4G"]
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [aysylu/loom "0.4.1"]
                 [org.clojure/math.numeric-tower "0.0.2"]]
  :main aco-tsp.core
  :aot [loom.graph])
