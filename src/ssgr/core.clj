(ns ssgr.core
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [ssgr.parser :as p]
            [ssgr.renderer :as r]
            [hiccup.core :as h])
  (:gen-class))

(defn write-file! [path contents]
  (let [components (fs/components path)]
    (fs/create-dirs (apply fs/path (drop-last components)))
    (spit (fs/file path) contents)))

(defn process-file! [path out]
  (try
    (let [text (slurp (fs/file path))
          doc (p/parse text)
          hiccup (r/render doc)
          html (->> hiccup
                    (map #(h/html %))
                    (str/join "\n"))]
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
  
  (-main "doc" "out")
  )

(comment  
  (def src "doc")
  (def out "out")
  )