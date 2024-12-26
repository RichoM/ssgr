(ns ssgr.core
  (:require [clojure.java.io :as io]
            [babashka.fs :as fs]
            [nextjournal.markdown :as md]
            [nextjournal.markdown.transform :as md.transform]
            [markdown.core :refer [md-to-html md-to-html-string]]
            [petitparser.core :as pp]
            [middleware.compilation.parser :refer [parse]]
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

#_(defn process-file! [path out]
  (try
    (let [text (slurp (fs/file path))
          data (md/parse text)
          hiccup (md.transform/->hiccup data)]
      (write-file! (-> (fs/path out)
                       (fs/strip-ext)
                       (str ".hic"))
                   (pr-str hiccup)))
    (catch Exception ex
      (println (str "ERROR processing " path) ex))))

(defn -main
  [& [src out]]
  (println "Looking for markdown files on" src)
  (let [src (fs/path src)
        out (fs/path out)]
    (fs/delete-tree out)
    ; physical bits parser
    #_(doseq [path (->> (fs/glob (fs/path src) "**")
                      (filter #(= "uzi" (fs/extension %))))]
      (println)
      (println (str "Found: " path))
      (try
        (let [uzi (slurp (fs/file path))
              ast (parse uzi)]
          (println (str/replace-first path
                                      (str src)
                                      (str out)))

          (write-file! (fs/path (str/replace-first path (str src) (str out)))
                       (print-str ast)))
        (catch Exception ex
          (println "ERROR!" ex))))
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