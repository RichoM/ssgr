(ns ssgr.core
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.tools.cli :as cli]
            [ssgr.parser :as p]
            [ssgr.clojure :as c]
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
  (c/eval-file! (fs/file path)
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

(defmacro project-data [key]
  ; HACK(Richo): This macro allows to read the project.clj file at compile time
  `~(let [data (-> "project.clj" slurp read-string)
          name (str (nth data 1))
          version (nth data 2)
          rest (drop 3 data)]
      ((apply assoc {:name name, :version version} rest) key)))

(def project-name
  (let [description (project-data :description)
        version (project-data :version)]
    (format "%s (%s)" description version)))

(defn usage [options-summary]
  (->> [project-name
        ""
        "Usage: ssgr [options] in out"
        ""
        "Options:"
        options-summary]
       (str/join \newline)))

(defn error-msg [errors]
  (str "Error:\n\n"
       (str/join \newline errors)))

(def cli-options
  [["-d" "--debug"
    :default false]
   ["-v" "--verbose"
    :default false]
   ["-h" "--help"]])

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
  (require '[clj-async-profiler.core :as prof])

  (prof/profile
   (dotimes [i 100]
     (-main #_"-v" "test-files" "out")))

  (prof/profile
   {:event :alloc}
   (dotimes [i 50]
     (-main #_"-v" "test-files" "out")))


  (prof/serve-ui 8080)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Mac
  (user/time+ 30000 (-main "test-files" "out"))

  ; Time per call: 374.55 ms   Alloc per call: 238,451,654b   Iterations: 85   (without cache)
  ; Time per call: 302.02 ms   Alloc per call: 138,063,395b   Iterations: 105  (with cache)
  ; Time per call: 285.18 ms   Alloc per call: 137,827,990b   Iterations: 106  (with cache)


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; WINDOWS

  (user/time+ 30000 (-main "test-files" "out"))

  ; Without cache
  ; Time per call: 2,03 s   Alloc per call: 789.383.213b   Iterations: 15
  ; Time per call: 1,47 s   Alloc per call: 788.834.863b   Iterations: 21
  ; Time per call: 1,44 s   Alloc per call: 788.815.114b   Iterations: 21

  ; With cache
  ; Time per call: 953,32 ms   Alloc per call: 434.416.927b   Iterations: 33
  ; Time per call: 955,02 ms   Alloc per call: 434.397.790b   Iterations: 33
  ; Time per call: 935,76 ms   Alloc per call: 434.387.573b   Iterations: 34

  ; After removing petitparser from line parsers
  ; Time per call: 796,30 ms   Alloc per call: 254.175.497b   Iterations: 38
  ; Time per call: 689,66 ms   Alloc per call: 254.058.073b   Iterations: 45
  ; Time per call: 676,88 ms   Alloc per call: 254.052.411b   Iterations: 45
  ; Time per call: 708,28 ms   Alloc per call: 257.838.506b   Iterations: 43
  ; Time per call: 631,97 ms   Alloc per call: 257.706.644b   Iterations: 49
  )
