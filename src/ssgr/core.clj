(ns ssgr.core
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [ssgr.parser :as p]
            [ssgr.renderer :as r]
            [hiccup.core :as h])
  (:gen-class))

(defn copy-file! [src dest]
  (let [components (fs/components dest)]
    (fs/create-dirs (apply fs/path (drop-last components)))
    (fs/copy src dest)))

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
                    (map r/html)
                    (str/join "\n"))]
      (write-file! out html))
    (catch Exception ex
      (println (str "ERROR processing " path) ex))))

(defn -main
  [& [src out]]
  (println "Looking for files on" src)
  (let [src (fs/path src)
        out (fs/path out)]
    (fs/delete-tree out)
    (doseq [path (->> (fs/glob (fs/path src) "**")
                      (remove fs/directory?))]
      (println)
      (println (str "Found: " path))
      (if (= "md" (fs/extension path))
        (let [out-path (-> path
                           (str/replace-first (str src) (str out))
                           (fs/strip-ext)
                           (str ".html"))]
          (println out-path)
          (process-file! path out-path))
        (let [out-path (str/replace-first path (str src) (str out))]
          (println out-path)
          (copy-file! path out-path))))))

(comment
  
  (-main "doc" "out")
  (-main "D:\\RichoM\\rescuesim\\rescuesim-intro" "out")
  )

(comment  
  (def src "doc")
  (def out "out")
  )