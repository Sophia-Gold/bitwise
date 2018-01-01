(defproject bitwise "0.2.3"
  :description "a collection of bit twiddling functions for Clojure"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [primitive-math "0.1.6"]]
  :main ^:skip-aot bitwise.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
