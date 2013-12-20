(ns aco-tsp.experimenter
  (:require [clojure.set]
            [loom.graph :as loom])
  (:use [aco-tsp.core :only [solve aco-init-ants-fn aco-init-pheromones-fn]]
        [aco-tsp.graph :only [file->graph tour-cost]]
        [clojure.java.io :only [file]]))

(defn generate-constants-sets [max-antcount]
  (for [antcount (range 1 (inc max-antcount))
        beta (range 0 5 0.5)
        rho (range 0.1 1 0.1)
        cl [10]
        q-sub-0 (range 0.25 1 0.25)]
    {:antcount antcount,
     :beta beta,
     :rho rho,
     :cl cl,
     :q-sub-0 q-sub-0}))

(defn -main [filename max-antcount]
  (let [cities (file->graph (file filename))
        constants-sets (generate-constants-sets (Integer/parseInt max-antcount))]
    (doseq [[constants best-tour] (pmap #(list % (first (solve cities
                                                               aco-init-ants-fn
                                                               aco-init-pheromones-fn
                                                               %)))
                                        constants-sets)]
      (println constants best-tour (tour-cost cities best-tour)))
    (shutdown-agents)
    (System/exit 0)))
