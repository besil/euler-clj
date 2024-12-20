(ns euler-clj.core
  (:gen-class)
  (:require [euler-clj.problems]
            [clojure.tools.logging :as log]
            [clojure.string :as string]))

(defn -main
  [& args]
  (let [p (first args)]
    (log/debug "Running problem:" p)
    (try
      (time (let [res (eval (read-string (str "(euler-clj.problems/" p " " (string/join " " (rest args)) ")")))]
              (log/info "Problem" p "output:" res)))
      (catch Exception e
        (log/error e)
        (log/warn ["Problem " p " not found. Try p1"])))))

