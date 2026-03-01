(ns ssgr.core
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.tools.cli :as cli]
            [ssgr.parser :as p]
            [ssgr.clojure :as c]
            [ssgr.renderer :as r]
            [ssgr.eval :as e]
            [ssgr.watcher :as w]
            [ssgr.http :as http]
            [ssgr.utils :as u])
  (:gen-class))

(defn process-regular-file! [src dest _]
  (u/copy-file! src dest))

(defn process-clj-file! [path options]
  (c/eval-file! (fs/file path)
                options e/eval-form))

(defn process-cljmd-file! [path out options]
  (let [doc (p/parse-file (fs/file path)
                          options e/eval-form)
        hiccup (r/render doc e/eval-render)
        html (r/html hiccup)]
    (u/write-file! (str (fs/strip-ext out) ".html")
                   html)))

(defn process-md-file! [path out options]
  (let [doc (p/parse-file (fs/file path)
                          options nil)
        hiccup (r/render doc e/eval-render)
        html (r/html hiccup)]
    (u/write-file! (str (fs/strip-ext out) ".html")
                   html)))

(defn process! [input output options]
  (println "Looking for files on" (str input))
  (u/delete-all! output)
  (e/reset-callbacks!)
  (let [files (u/list-files input)
        begin-time (System/nanoTime)]
    (println "Found" (count files) "files.")
    (doseq [path files]
      (try
        (let [out-path (str/replace-first path 
                                          (str input) 
                                          (str output))]
          (when (:verbose options)
            (println (str input)))
          (case (fs/extension path)
            "md" (process-md-file! path out-path options)
            "cljmd" (process-cljmd-file! path out-path options)
            "clj" (process-clj-file! path options)
            (process-regular-file! path out-path options)))
        (catch Exception ex
          (println (str "ERROR processing " path) ex))))
    (let [end-time (System/nanoTime)]
      (println "DONE!")
      (println "Elapsed time:"
               (/ (double (- end-time begin-time))
                  1000000.0)
               "ms"))))

(defn start-watcher! [input output options]
  (w/start-watcher!
   input
   (fn [_evt _child]
     (println)
     (process! input output options))))

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
        "Usage: ssgr [options]"
        ""
        "Options:"
        options-summary]
       (str/join \newline)))

(defn error-msg [errors]
  (str "Error:\n\n"
       (str/join \newline errors)))



(def cli-options
  [["-i" "--input PATH" "Path to input files"
    :missing "Input path is required"
    :parse-fn fs/path
    :validate [#(and (fs/directory? %) (fs/exists? %))
               "Must be an existing directory"]]
   ["-o" "--output PATH" "Path to output files"
    :missing "Output path is required"
    :parse-fn fs/path]
   ["-s" "--serve PORT" "Port number"
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]]
   ["-w" "--watch"
    :default false]
   ["-d" "--debug"
    :default false]
   ["-v" "--verbose"
    :default false]
   ["-h" "--help"]])

(defn validate-args [args]
  (let [{:keys [ errors options summary]} (cli/parse-opts args cli-options)]
    (cond
      (:help options)
      {:exit-message (usage summary) :ok? true}

      errors
      {:exit-message (error-msg errors)}

      :else
      {:options options})))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn -main [& args]
  (let [{:keys [options exit-message ok?]} (validate-args args)]
    (if exit-message
      (exit (if ok? 0 1) exit-message)
      (let [{:keys [input output watch serve]} options]
        (process! input output options)
        (when watch
          (start-watcher! input output options))
        (when serve
          (http/start-server! output serve))))))

(comment
  (-main )
  (-main "-i" "doc" 
         "-o" "out"
         ;"-s" "8081"
         )
  

  

  )