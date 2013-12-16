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

(defn solve [graph antcount init-ants-fn init-pheromone-fn]
  (let [ants (init-ants-fn graph antcount)]
       [pheromone (init-pheromone-fn graph)]
       (loop [time 0
	      ants ants
	      pheromone pheromone]
	     )))

; returns chosen tour
(defn ant-tour [start graph pheromones transition-fn]
      (let [ant-tour-helper (fn [current visited unvisited traversed-edges]
				(case [(and (= current start)
					    (empty? unvisited))
					    visited]
				      [else (let [next (transition-fn current visited graph pheromones)]
						 (ant-tour-helper next
								  (append visited (list next))
								  (disj unvisited next))]))]
	   (ant-tour-helper start (list start) (nodes graph) (list))))

; p^k[i,j](t)
; i : node
; j : node
; unvisited : set of node
; pheromones : (node node) -> int
; sight : (node node) -> int
(defn as-transition-rule [i j unvisited pheromones sight alpha beta]
  (/ (* (exp (pheromones i j) alpha)
	(exp (sight i j) beta)
     (apply + (map (fn [unvisited-node] (* (exp (pheromones i unvisited-node) alpha)
				     (exp (sight i unvisited-node) beta)))
	       unvisited)))))

(defn acs-transition-rule [i j unvisited pheromones sight beta]
  (as-transition-rule i j unvisited pheromones sight 1 beta))

(defn init-sight [graph]
  (let [sight (fn [i j]
		  (/ 1
		     (weight graph i j)))]
    sight))

