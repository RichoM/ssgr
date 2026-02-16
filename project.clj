(defproject ssgr "1.1"
  :description "Static Site Generator by Richo"
  :url "https://github.com/RichoM/ssgr"
  :license {:name "MIT"
            :url "https://mit-license.org/"}
  :dependencies [[org.clojure/clojure "1.12.0"]
                 [hiccup "1.0.5"]
                 [babashka/fs "0.5.23"]
                 [org.babashka/sci "0.9.44"]
                 [org.clojure/tools.cli "1.1.230"]]
  :main ssgr.core
  :target-path "target/%s"
  :java-source-paths ["src"]
  :profiles {:dev {:dependencies [[org.clojure/core.async "0.6.532"]
                                  [org.clojars.beppu/clj-audio "0.3.0"]
                                  [criterium "0.4.6"]
                                  [com.taoensso/tufte "2.7.0"]
                                  [djblue/portal "0.62.2"]
                                  [dev.weavejester/hashp "0.4.0"]
                                  [com.clojure-goes-fast/clj-async-profiler "1.6.2"]]
                   :injections [(require 'hashp.preload)]
                   :resource-paths ["sounds"]
                   :source-paths ["dev"]
                   :plugins [[com.jakemccrary/lein-test-refresh "0.24.1"]]
                   :global-vars {*unchecked-math* :warn-on-boxed
                                 *warn-on-reflection* true}
                   :jvm-opts [; These JVM opts are required by clj-async-profiler
                              "-Djdk.attach.allowAttachSelf"
                              "-XX:+UnlockDiagnosticVMOptions"
                              "-XX:+DebugNonSafepoints"]}
             :uberjar {:aot [ssgr.core]
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"
                                  "-Dclojure.compiler.elide-meta=[:doc :file :line :added]"]}})
