(ns aco-tsp.core
    (:use [loom.graph]))

(defn init-pheromones [g]
  (reduce (fn [m elem] (assoc m elem 0 ))
          {}
          (edges g)))

(defn add-pheromone [amt edge m]
  (update-in m [edge] (partial + amt)))

(defn decay-one-pheromone [decay-fn edge m]
  (update-in m [edge] (comp (partial max 0) decay-fn)))

(defn decay-pheromones [decay-fn m]
  (reduce (fn [m e]
            (update-in m [e] (comp (partial max 0) decay-fn))) m (keys m)))
