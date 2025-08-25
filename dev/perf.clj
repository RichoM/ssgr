(ns perf
  (:require [ssgr.utils :as u]
            [babashka.fs :as fs]
            [ssgr.parser :as p]
            [ssgr.eval :as e]
            [user :refer [time+]]))

(def test-file "dev/perf.md")

(defn merge-test-files [input-path out-file]
  (doseq [file (filter (fn [f]
                         (#{"md" "cljmd"} (fs/extension f)))
                       (u/list-files input-path))]
    (let [contents (slurp file)]
      (spit out-file contents :append true)
      (spit out-file "\r\n\r\n" :append true))))
  
(defn benchmark [cljmd?]  
  (let [src (slurp test-file)
        options {}
        eval-form (if cljmd? e/eval-form nil)]
    (println "Parsing in"
             (if cljmd? "CLJMD" "MD")
             "mode...")
    (time+ 30000
           (count (:blocks (p/parse src options eval-form))))))
  

; Time per call: 104,82 ms   Alloc per call: 85.106.833b   Iterations: 289
; Time per call: 79,39 ms   Alloc per call: 60.207.669b   Iterations: 386

(comment
  (benchmark true)
  )