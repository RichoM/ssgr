(ns perf
  (:require [ssgr.utils :as u]
            [babashka.fs :as fs]
            [ssgr.parser :as p]
            [ssgr.eval :as e]
            [ssgr.renderer :as r]
            [user :refer [time+]]
            [clj-async-profiler.core :as prof]))

(def test-file "dev/perf.md")

(defn merge-test-files [input-path out-file]
  (doseq [file (filter (fn [f]
                         (#{"md" "cljmd"} (fs/extension f)))
                       (u/list-files input-path))]
    (let [contents (slurp file)]
      (spit out-file contents :append true)
      (spit out-file "\r\n\r\n" :append true))))
  
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
  (benchmark-parse true)
  (benchmark-render true)

  ; Time per call: 11,02 ms   Alloc per call: 9.146.703b   Iterations: 2737
  ; Time per call: 11,06 ms   Alloc per call: 9.146.816b   Iterations: 2773
  ; Time per call: 10,96 ms   Alloc per call: 9.146.816b   Iterations: 2813
  
  (profile true)

  (prof/serve-ui 8080)
  )