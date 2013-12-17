(ns aco-tsp.core
  (:require [clojure.set]
            [loom.graph :as loom])
  (:use [aco-tsp.core :only [solve aco-init-ants-fn aco-init-pheromones-fn]]
        [clojure.java.io :only [file]]))

(defn generate-constants-sets []
  (for [antcount (range 0 3)
	beta (range 0 5 .5)
	rho [0.1]
	cl [10]
	q-sub-0 [.5]]
       {:antcount antcount,
	:beta beta,
	:rho rho,
	:cl cl,
	:q-sub-0 q-sub-0}))

(defn -main []
  (loop [constants-sets (generate-constants-sets)]
	(let [constants (first constants-sets)
	      rest-constants (rest constants-sets)]
	  (println (solve aco-init-ants-fn
			  aco-init-pheromones-fn
			  constants))
	  (recur rest-constants))))
