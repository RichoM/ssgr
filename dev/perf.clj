(ns perf
  (:require [babashka.fs :as fs]
            [ssgr.parser :as p]
            [ssgr.eval :as e]
            [ssgr.renderer :as r]
            [user :refer [time+]]
            [clj-async-profiler.core :as prof]))

(def test-file "dev/perf.md")

(defn benchmark-parse [cljmd?]
  (let [src (slurp test-file)
        options {}
        eval-form (if cljmd? e/eval-form nil)]
    (println "Parsing in"
             (if cljmd? "CLJMD" "MD")
             "mode...")
    (time+ 30000
           (count (:blocks (p/parse src options eval-form))))))

(defn benchmark-render [cljmd?]
  (let [src (slurp test-file)
        options {}
        eval-form (if cljmd? e/eval-form nil)
        doc (p/parse src options eval-form)]
    (println "Rendering in"
             (if cljmd? "CLJMD" "MD")
             "mode...")
    (time+ 30000
           (r/render doc e/eval-render))))

(defn profile [cljmd?]
  (let [src (slurp test-file)
        options {}
        eval-form (if cljmd? e/eval-form nil)]
    (println "Parsing in"
             (if cljmd? "CLJMD" "MD")
             "mode...")
    (prof/profile
     (dotimes [_ 100]
       (let [doc (p/parse src options eval-form)
             hiccup (r/render doc e/eval-render)
             html (r/html hiccup)]
         (count html))))))

(comment
  (benchmark-parse false)
  (benchmark-render true)

  ; Parsing in CLJMD mode...
  ; Time per call: 41.94 ms   Alloc per call: 153,722,507b   Iterations: 736
  ; Time per call: 41.60 ms   Alloc per call: 153,715,794b   Iterations: 743
  ; Time per call: 41.86 ms   Alloc per call: 153,710,955b   Iterations: 737


  ; Parsing in MD mode...
  ; Time per call: 27.92 ms   Alloc per call: 113,890,383b   Iterations: 1101
  ; Time per call: 28.10 ms   Alloc per call: 113,890,312b   Iterations: 1072
  ; Time per call: 28.21 ms   Alloc per call: 113,890,301b   Iterations: 1117

  (profile true)


  (prof/serve-ui 8080)
  )