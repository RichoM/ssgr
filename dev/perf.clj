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
  (benchmark-parse false)
  (benchmark-render true)

  ; Parsing in CLJMD mode...
  ; Time per call: 9.17 ms   Alloc per call: 53,185,700b   Iterations: 3277
  ; Time per call: 9.04 ms   Alloc per call: 53,180,058b   Iterations: 3477
  ; Time per call: 9.10 ms   Alloc per call: 53,180,208b   Iterations: 3411
  
  ; Parsing in MD mode...
  ; Time per call: 4.58 ms   Alloc per call: 13,430,412b   Iterations: 6799
  ; Time per call: 4.54 ms   Alloc per call: 13,430,344b   Iterations: 6841
  ; Time per call: 4.54 ms   Alloc per call: 13,430,344b   Iterations: 6626
  
  (profile true)

  (prof/serve-ui 8080)
  )