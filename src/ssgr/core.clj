(ns ssgr.core
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.tools.cli :as cli]
            [ssgr.parser :as p]
            [ssgr.clojure :as c]
            [ssgr.renderer :as r]
            [ssgr.eval :as e]
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

(defn process! [src out options]
  (println "Looking for files on" (str src))
  (u/delete-all! out)
  (e/reset-callbacks!)
  (let [files (u/list-files src)
        begin-time (System/nanoTime)]
    (println "Found" (count files) "files.")
    (doseq [path files]
      (try
        (let [out-path (str/replace-first path (str src) (str out))]
          (when (:verbose options)
            (println (str src)))
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

  ; After removing petitparser from line parsers
  ; Time per call: 385.87 ms   Alloc per call: 86,816,150b   Iterations: 79
  ; Time per call: 257.25 ms   Alloc per call: 86,741,207b   Iterations: 121
  ; Time per call: 254.40 ms   Alloc per call: 86,738,459b   Iterations: 121
  ; Time per call: 252.06 ms   Alloc per call: 86,736,994b   Iterations: 122


  ; After removing petitparser from line-prefix
  ; Time per call: 233.03 ms   Alloc per call: 50,860,398b   Iterations: 136
  ; Time per call: 223.10 ms   Alloc per call: 50,858,196b   Iterations: 144
  ; Time per call: 206.81 ms   Alloc per call: 50,855,321b   Iterations: 155
  ; Time per call: 206.47 ms   Alloc per call: 50,854,240b   Iterations: 157

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

  ; After removing petitparser from parse-line-breaks!
  ; Time per call: 507,74 ms   Alloc per call: 146.756.605b   Iterations: 61
  ; Time per call: 608,95 ms   Alloc per call: 146.761.263b   Iterations: 55
  ; Time per call: 517,32 ms   Alloc per call: 146.745.054b   Iterations: 61
  ; Time per call: 490,19 ms   Alloc per call: 146.748.615b   Iterations: 66

  ; After removing petitparser from line-prefix
  ; Time per call: 606,54 ms   Alloc per call: 145.854.363b   Iterations: 50
  ; Time per call: 516,06 ms   Alloc per call: 145.756.805b   Iterations: 61
  ; Time per call: 507,82 ms   Alloc per call: 145.743.720b   Iterations: 61
  ; Time per call: 506,00 ms   Alloc per call: 145.728.546b   Iterations: 61

  ; After replacing input-stream for deftype StringStream
  ; Time per call: 551,52 ms   Alloc per call: 139.474.826b   Iterations: 61
  ; Time per call: 501,07 ms   Alloc per call: 139.465.003b   Iterations: 64
  ; Time per call: 508,33 ms   Alloc per call: 139.459.286b   Iterations: 61

  ; After first iteration with the lexer  
  ; Time per call: 437,91 ms   Alloc per call: 71.697.405b   Iterations: 71
  ; Time per call: 443,83 ms   Alloc per call: 71.685.944b   Iterations: 71
  ; Time per call: 451,56 ms   Alloc per call: 71.681.904b   Iterations: 67
  ; Time per call: 427,29 ms   Alloc per call: 71.681.319b   Iterations: 73
  )

