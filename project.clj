(defproject ssgr "0.1.0-SNAPSHOT"
  :description "Static Site Generator by Richo"
  :url "https://github.com/RichoM/ssgr"
  :license {:name "MIT"
            :url "https://mit-license.org/"}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [io.github.nextjournal/markdown "0.6.157"]
                 [markdown-clj "1.12.2"]
                 [hiccup "1.0.5"]
                 [clj-petitparser "0.1.2-SNAPSHOT"]
                 [babashka/fs "0.5.23"]]
  :main ^:skip-aot ssgr.core
  :target-path "target/%s"
  :profiles {:dev {:global-vars {*unchecked-math* :warn-on-boxed
                                 *warn-on-reflection* true}}
             :uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"
                                  "-Dclojure.compiler.elide-meta=[:doc :file :line :added]"]}})
