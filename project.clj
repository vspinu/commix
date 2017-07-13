(defproject commix "0.1.0"
  :description "Micro-framework for data-driven composable system architectures"
  :url "https://github.com/vspinu/commix"
  :license {:name "The MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [com.stuartsierra/dependency "0.2.0"]]
  :profiles {:dev
             {:dependencies [[org.clojure/clojure "1.8.0"]
                             [org.clojure/clojurescript "1.7.228"]
                             ;; [org.clojure/clojurescript "1.9.183"]
                             [com.cemerick/piggieback "0.2.2"]]}
             }
  :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}
  :plugins [[lein-doo "0.1.7"]]
  :cljsbuild
  {:builds [{:id "test-phantom"
             :source-paths ["src" "test"]
             :compiler {:output-to  "target/cljs/test-phantom/test-commix.js"
                        :output-dir "target/cljs/test-phantom/out"
                        :main commix.test-runner
                        :optimizations :none}}
            {:id "test-nashorn"
             :source-paths ["src" "test"]
             :compiler {:output-to  "target/cljs/test-nashorn/test-commix.js"
                        :output-dir "target/cljs/test-nashorn/out"
                        :main commix.test-runner
                        :optimizations :simple}}
            {:id "test-node"
             :source-paths ["src" "test"]
             :compiler {:target :nodejs
                        :output-to  "target/cljs/test-node/test-commix.js"
                        :output-dir "target/cljs/test-node/out"
                        :main commix.test-runner
                        :optimizations :none}}]}
  :aliases {"test-phantom" ["doo" "phantom" "test-phantom" "once"]
            "test-nashorn" ["doo" "nashorn" "test-nashorn" "once"]
            "test-node"    ["doo" "node" "test-node" "once"]
            "test-cljs"    ["do" ["test-phantom"] ["test-nashorn"] ["test-node"]]
            "test-all"     ["do" ["test"] ["test-cljs"]]
            "test"         ["test" ":only" "commix.core-test"]})
