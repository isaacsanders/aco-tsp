(ns aco-tsp.core
  (:require [clojure.set :as :set])
  (:use [loom.graph]
        [clojure.contrib.math :only [exp]]))

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
           best-tour nil
           pheromone pheromone]
      (if (> time 100000)
        (list best-tour pheromone) ; return
        (let [output (do-transition graph ants pheromone)]
          (recurse (+ 1 time) (first output) (last output)))))))

(defn do-transition [graph ants pheromone]
  (let [tours (map (fn [ant] (ant-tour ant graph pheromone ??transition-fn??))
                   ants)]
    [new-pheromones (change-pheromones tours pheromones)]
    [best-tour (get-best-tour tours)]
    (list best-tour new-pheromones)))

; returns chosen tour
(defn ant-tour [start graph pheromones transition-fn]
  (loop [current start
	 visited (list start)
	 unvisited (nodes graph)
	 traversed-edges (list)]
	(case [(and (= current start)
		    (empty? unvisited))
	       visited]
	      [else (let [next (transition-fn current visited graph pheromones)]
		      (recur next
			     (append visited (list next))
			     (disj unvisited next)
			     (append traversed-edges (list [current next]))))])))

; p^k[i,j](t)
; i : node
; j : node
; unvisited : set of node
; pheromones : (node node) -> int
; sight : (node node) -> int
; returns : probability of choosing node j as the next node
(defn as-transition-rule [i j unvisited pheromones sight alpha beta]
  (/ (* (exp (pheromones i j) alpha)
        (exp (sight i j) beta)
        (apply + (map (fn [unvisited-node] (* (exp (pheromones i unvisited-node) alpha)
                                              (exp (sight i unvisited-node) beta)))
                      unvisited)))))

(defn acs-transition-rule [i j unvisited pheromones sight beta]
  (as-transition-rule i j unvisited pheromones sight 1 beta))

(def cl 10)
(def q-sub-0 0.5)
(def beta 1)

(defn visibility [g i j]
  (/ 1.0 (weight g i j)))

(defn tau-eta [g p i j]
  (* (p i j)
     (exp (visibility g i j) beta)))

(defn choose-next-city [graph pheromones current previous]
  (let [candidates (set/difference (set (successors graph current)) (set previous))
        pheromones-fn (fn [candidate] (tau-eta graph pheromones current candidate))
        chance-fn (fn [probs]
                    (let [chance (rand)]
                      (loop [[prob state] (first probs)
                             probs (rest probs)]
                        (if (< chance prob)
                          state
                          (let [[next-prob next-state] (first probs)]
                            (recur [(+ prob next-prob) next-state] (rest probs)))))))
        probs-fn (fn [probs candidates]
                   (let [candidate (first candidates)
                         normalizer (apply + (map (partial tau-eta graph pheromones current) candidates))
                         candidates (rest candidates)]
                     (if (empty? candidates)
                       probs
                       (recur (assoc probs (/ (tau-eta graph pheromones current candidate) normalizer)
                                     candidate)
                              (rest candidates)))))
        probability-fn (fn [candidates]
                         (chance-fn (probs-fn {} candidates)))]
    (if (<= (rand) q-sub-0)
      (max-key pheromones-fn candidates)
      (probability-fn candidates))))
