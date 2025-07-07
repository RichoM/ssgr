(ns ssgr.core
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.tools.cli :as cli]
            [ssgr.parser :as p]
            [ssgr.renderer :as r]
            [ssgr.eval :as e])
  (:gen-class))

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

(defn copy-file! [src dest options]
  (when (:verbose options)
    (println (str src)))
  (when-let [parent (fs/parent dest)]
    (fs/create-dirs parent))
  (fs/copy src dest))

(defn process-clj-file! [path options]
  (when (:verbose options)
    (println (str path)))
  (p/parse-file (fs/file path)
                options e/eval-form))

(defn process-cljmd-file! [path out options]
  (when (:verbose options)
    (println (str path)))
  (let [doc (p/parse-file (fs/file path)
                          options e/eval-form)
        hiccup (r/render doc e/eval-render)
        html (r/html hiccup)]
    (write-file! (str (fs/strip-ext out) ".html")
                 html)))

(defn process-md-file! [path out options]
  (when (:verbose options)
    (println (str path)))
  (let [doc (p/parse-file (fs/file path)
                          options nil)
        hiccup (r/render doc e/eval-render)
        html (r/html hiccup)]
    (write-file! (str (fs/strip-ext out) ".html")
                 html)))

(defn list-files [src]
  (when-let [entries (sort (.listFiles (fs/file src)))]
    (concat (filter fs/regular-file? entries)
            (->> entries
                 (filter fs/directory?)
                 (mapcat list-files)))))

(defn process! [src out options]
  (println "Looking for files on" (str src))
  (delete-all! out)
  (e/reset-callbacks!)
  (let [files (list-files src)
        begin-time (System/nanoTime)]
    (println "Found" (count files) "files.")
    (doseq [path files]
      (try
        (let [out-path (str/replace-first path (str src) (str out))]
          (case (fs/extension path)
            "md" (process-md-file! path out-path options)
            "cljmd" (process-cljmd-file! path out-path options)
            "clj" (process-clj-file! path options)
            (copy-file! path out-path options)))
        (catch Exception ex
          (println (str "ERROR processing " path) ex))))
    (let [end-time (System/nanoTime)]
      (println "DONE!")
      (println "Elapsed time:"
               (/ (double (- end-time begin-time))
                  1000000.0)
               "ms"))))

(def cli-options
  [["-d" "--debug"
    :default false]
   ["-v" "--verbose"
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

  (-main "-v" "test-files" "out")

  (require '[taoensso.tufte :as tufte])
  (tufte/add-basic-println-handler! {})
  (tufte/profile
   {}
   (-main "test-files" "out"))

  @tufte/pstats
  )
