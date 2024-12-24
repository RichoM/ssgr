(defproject ssgr "0.1.0-SNAPSHOT"
  :description "Static Site Generator by Richo"
  :url "https://github.com/RichoM/ssgr"
  :license {:name "MIT"
            :url "https://mit-license.org/"}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [io.github.nextjournal/markdown "0.6.157"]]
  :main ^:skip-aot ssgr.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
