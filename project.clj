(defproject bitwise "0.1.0"
  :description "a collection of bit twiddling functions for Clojure"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot bitwise.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
