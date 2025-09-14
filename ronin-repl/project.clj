(defproject ronin-repl "0.1.0-SNAPSHOT"
  :description "RONIN REPL - Interactive RONIN Language Environment"
  :url "https://github.com/kae3g/ronin-compiler"
  :license {:name "Apache License 2.0"
            :url "http://www.apache.org/licenses/LICENSE-2.0"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [humble.ui/humble.ui "0.9.0"]
                 [clj-http "3.12.3"]
                 [cheshire "5.11.0"]
                 [clojure.java.io "1.0.0"]
                 [clojure.string "1.0.0"]]
  :main ronin-repl.core
  :aot [ronin-repl.core]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
