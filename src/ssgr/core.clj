(ns ssgr.core
  (:require [clojure.java.io :as io]
            [babashka.fs :as fs]
            [nextjournal.markdown :as md]
            [nextjournal.markdown.transform :as md.transform])
  (:gen-class))

(defn write-file! [path contents]
  (let [components (fs/components path)]
    (fs/create-dirs (apply fs/path (drop-last components)))
    (spit (fs/file path) contents)))

(defn process-file! [path out]
  (try
    (let [text (slurp (fs/file path))
          data (md/parse text)
          hiccup (md.transform/->hiccup data)]
      (write-file! (fs/path out path)
                   (pr-str hiccup)))
    (catch Exception ex
      (println (str "ERROR processing " path) ex))))

(defn -main
  "I don't do a whole lot ... yet."
  [& [src out]]
  (println "Looking for markdown files on" src)
  (let [src (fs/path src)
        out (fs/path out)]
    (doseq [path (fs/list-dir src)]
      (when (= "md" (fs/extension path))
        (println (str "Found: " path))
        (process-file! path out)))))

(comment
  
  (def src "doc")
  (def out "out")
  )