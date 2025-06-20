(defproject ssgr "0.1.0-SNAPSHOT"
  :description "Static Site Generator by Richo"
  :url "https://github.com/RichoM/ssgr"
  :license {:name "MIT"
            :url "https://mit-license.org/"}
  :dependencies [[org.clojure/clojure "1.12.0"]
                 [markdown-clj "1.12.2"]
                 [hiccup "1.0.5"]
                 [clj-petitparser "0.1.3"]
                 [babashka/fs "0.5.23"]
                 [org.babashka/sci "0.9.44"]
                 [org.clojure/tools.cli "1.1.230"]]
  :main ssgr.core
  :target-path "target/%s"
  :profiles {:dev {:dependencies [[org.clojure/core.async "0.6.532"]
                                  [org.clojars.beppu/clj-audio "0.3.0"]
                                  [djblue/portal "0.59.1"]]
                   :resource-paths ["sounds"]
                   :plugins [[com.jakemccrary/lein-test-refresh "0.24.1"]]
                   :global-vars {*unchecked-math* :warn-on-boxed
                                 *warn-on-reflection* true}}
             :uberjar {:aot [ssgr.core]
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"
                                  "-Dclojure.compiler.elide-meta=[:doc :file :line :added]"]}})
