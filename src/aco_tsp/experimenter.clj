(ns aco-tsp.experimenter
  (:require [clojure.set]
            [loom.graph :as loom])
  (:use [aco-tsp.core :only [solve aco-init-ants-fn aco-init-pheromones-fn]]
        [aco-tsp.graph :only [file->graph tour-cost]]
        [clojure.java.io :only [file]]))

(defn generate-constants-sets []
  (for [antcount (range 1 3)
        beta (range 0 5 0.5)
        rho [0.1]
        cl [10]
        q-sub-0 [0.5]]
    {:antcount antcount,
     :beta beta,
     :rho rho,
     :cl cl,
     :q-sub-0 q-sub-0}))

(defn -main [filename]
  (let [cities (file->graph (file filename))]
    (loop [constants-sets (generate-constants-sets)]
      (if (empty? constants-sets)
        (do
          (shutdown-agents)
          (System/exit 0))
        (let [constants (first constants-sets)
              rest-constants (rest constants-sets)
              best-tour (first (solve cities
                                      aco-init-ants-fn
                                      aco-init-pheromones-fn
                                      constants))]
          (do
            (println constants best-tour (tour-cost cities best-tour))
            (recur rest-constants)))))))
