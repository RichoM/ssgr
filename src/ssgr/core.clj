(ns ssgr.core
  (:require [babashka.fs :as fs]
            [markdown.core :as md :refer [md-to-html-string]]
            [clojure.string :as str])
  (:gen-class))

(defn write-file! [path contents]
  (let [components (fs/components path)]
    (fs/create-dirs (apply fs/path (drop-last components)))
    (spit (fs/file path) contents)))

(defn process-file! [path out]
  (try
    (let [text (slurp (fs/file path))
          html (md-to-html-string text)]
      (write-file! out html))
    (catch Exception ex
      (println (str "ERROR processing " path) ex))))

(defn -main
  [& [src out]]
  (println "Looking for markdown files on" src)
  (let [src (fs/path src)
        out (fs/path out)]
    (fs/delete-tree out)
    (doseq [path (->> (fs/glob (fs/path src) "**")
                      (filter #(= "md" (fs/extension %))))]
      (println)
      (println (str "Found: " path))
      (let [out-path (-> path
                         (str/replace-first (str src) (str out))
                         (fs/strip-ext)
                         (str ".html"))]
        (println out-path)
        (process-file! path out-path)))))

(comment  
  (def src "doc")
  (def out "out")
  )