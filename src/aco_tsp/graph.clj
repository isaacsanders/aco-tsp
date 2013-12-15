(ns aco-tsp.graph
  (:require [clojure.string]
            [clojure.set])
  (:use [clojure.java.io :only [reader file]]
        [loom.graph]
        [loom.io]))

(defn file->graph [f]
  (let [reader-handle (reader f)]
    (loop [line (.readLine reader-handle)
           graph (weighted-graph)]
      (if (nil? line)
        graph
        (let [edge (map (fn [a] (Integer/parseInt a))
                                        (clojure.string/split line #" "))]
          (recur (.readLine reader-handle) (add-edges graph edge)))))))

(defn tour-cost [g tour]
  (loop [current-city (first tour)
         tour (rest tour)
         distance 0]
    (let [next-city (first tour)]
      (if (empty? (rest tour))
        (+ distance (weight g current-city next-city))
        (recur next-city
               (rest tour)
               (+ distance (weight g current-city next-city)))))))

(defn nearest-neighbor-heuristic [g]
  (loop [current-city (first (nodes g))
         unvisited-cities (nodes g)
         tour [current-city]]
    (let [unvisited-neighbors (clojure.set/intersection (set unvisited-cities)
                                                        (set (successors g current-city)))
          next-city (first (sort-by (partial weight g current-city) unvisited-neighbors))]
      (if (empty? unvisited-cities)
        (tour-cost g tour)
        (recur next-city
               (disj unvisited-cities next-city)
               (conj tour next-city))))))
