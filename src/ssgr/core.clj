(ns ssgr.core
  (:require [babashka.fs :as fs]
            [markdown.core :as md :refer [md-to-html-string]]
            [clojure.string :as str]
            [sci.core :as sci]
            [clojure.java.io :as io]
            [edamame.core :as e]
            [clojure.tools.reader.reader-types :as r])
  (:import [clojure.tools.reader.reader_types SourceLoggingPushbackReader])
  (:gen-class))

(comment

  (def ;^SourceLoggingPushbackReader
    r (-> (io/file "project.clj")
          ;(io/reader)
          (slurp)
          (e/source-reader)))

  (r/unread r \()
  (r/peek-char r)
  (r/read-char r)
  (.read_char r)
  [(e/get-line-number r)
   (e/get-column-number r)]

  (do (try
        (when-let [ch (r/read-char r)]
          (if (= ch \()
            (do (r/unread r ch)
                (let [obj (e/parse-next r #_(e/normalize-opts {:all false}))]
                  (println "SUCCESS!" obj)
                  (println (meta obj))))
            (println ch)))        
        (catch Exception ex
          (println "ERROR!" (ex-message ex))))
      [(e/get-line-number r)
       (e/get-column-number r)])

  (.close r)

  (e/parse-string "richo")

  )

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