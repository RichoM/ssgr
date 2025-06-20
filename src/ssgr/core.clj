(ns ssgr.core
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.tools.cli :as cli]
            [ssgr.parser :as p]
            [ssgr.renderer :as r]
            [ssgr.eval :as e])
  (:gen-class))

(defn copy-file! [src dest]
  (when-let [parent (fs/parent dest)]
    (fs/create-dirs parent))
  (fs/copy src dest))

(defn write-file! [path contents]
  (when-let [parent (fs/parent path)]
    (fs/create-dirs parent))
  (spit (fs/file path) contents))

(defn delete-path! [path]
  (try
    (fs/delete path)
    (catch Exception ex
      (println "Couldn't delete file" (ex-message ex)))))

(defn delete-all! [target-dir]
  (fs/walk-file-tree target-dir
                     {:visit-file (fn [path _]
                                    (delete-path! path)
                                    :continue)
                      :post-visit-dir (fn [path _]
                                        (delete-path! path)
                                        :continue)}))

(defn process-file! [path out options]
  (try
    (let [text (slurp (fs/file path))
          doc (p/parse text options)
          hiccup (r/render doc)
          html (r/html hiccup)]
      (when out 
        (write-file! out html)))
    (catch Exception ex
      (println (str "ERROR processing " path) ex))))

(defn list-files [src]
  (when-let [entries (sort (.listFiles (fs/file src)))]
    (lazy-seq (concat (filter fs/regular-file? entries)
                      (->> entries
                           (filter fs/directory?)
                           (mapcat list-files))))))

(defn process! [src out options]
  (println "Looking for files on" (str src))
  (delete-all! out)
  (e/reset-callbacks!)
  (let [files (list-files src)
        begin-time (System/nanoTime)]
    (println "Found" (count files) "files.")
    (doseq [path files]
      (case (fs/extension path)
        "md" (let [out-path (-> path
                                (str/replace-first (str src) (str out))
                                (fs/strip-ext)
                                (str ".html"))]
               (process-file! path out-path options))
        "clj" (process-file! path nil options)
        (let [out-path (str/replace-first path (str src) (str out))]
          (copy-file! path out-path))))
    (let [end-time (System/nanoTime)]
      (println "DONE!")
      (println "Elapsed time:"
               (/ (double (- end-time begin-time))
                  1000000.0)
               "ms"))))

(def cli-options
  [["-d" "--debug"
    :default false]
   ["-h" "--help"]])

(defn usage [options-summary]
  (->> ["Static Site Generator by Richo (SSGR)"
        ""
        "Usage: ssgr [options] in out"
        ""
        "Options:"
        options-summary]
       (str/join \newline)))

(defn error-msg [errors]
  (str "Error:\n\n"
       (str/join \newline errors)))

(defn validate-args [args]
  (let [{:keys [arguments errors options summary]} (cli/parse-opts args cli-options)]
    (cond
      (:help options)
      {:exit-message (usage summary) :ok? true}

      errors
      {:exit-message (error-msg errors)}

      (>= (count arguments) 1)
      (let [[src out] arguments]
        {:src (fs/path src)
         :out (fs/path (or out "out"))
         :options options})
      
      :else
      {:exit-message (usage summary)})))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn -main [& args]
  (let [{:keys [src out options exit-message ok?]} (validate-args args)]
    (if exit-message
      (exit (if ok? 0 1) exit-message)
      (process! src out options))))

(comment
  (use 'criterium.core)

  (-main "test-files" "out")

  (require '[taoensso.tufte :as tufte])
  (tufte/add-basic-println-handler! {})
  (tufte/profile
   {}
   (-main "test-files" "out"))

  @tufte/pstats
  )
