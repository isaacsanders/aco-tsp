(ns aco-tsp.core
  (:require [clojure.set])
  (:use [loom.graph]
        [clojure.math.numeric-tower :only [expt]]))

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

(def cl 10)
(def q-sub-0 0.5)
(def beta 1)

(defn visibility [g i j]
  (/ 1.0 (weight g i j)))

(defn tau-eta [g p i j]
  (* (p i j)
     (expt (visibility g i j) beta)))

(defn choose-next-city [graph pheromones current previous]
  (let [candidates (take cl (sort-by #(weight graph current %)
                                     (clojure.set/difference (set (successors graph current))
                                                             (set previous))))
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
                         normalizer (apply + (map (partial tau-eta graph pheromones current) candidates))]
                     (if (empty? candidates)
                       probs
                       (recur (assoc probs (/ (tau-eta graph pheromones current candidate) normalizer)
                                     candidate)
                              (rest candidates)))))
        probability-fn (fn [candidates]
                         (chance-fn (probs-fn {} candidates)))]
    (case
      (empty? candidates) (apply (partial max-key #(weight graph current)) (successors graph current))
      (<= (rand) q-sub-0) (apply (partial max-key pheromones-fn) candidates)
      :else (probability-fn candidates))))

; returns chosen tour
(defn next-step [[current tour] graph pheromones]
  (let [unvisited (clojure.set/difference (set (nodes graph)) (set tour))]
    (case
      (nil? current) [current tour]
      (and (= current (first tour)) (empty? unvisited)) [nil (concat tour [current])]
      :else (let [next-state (choose-next-city graph pheromones current tour)]
              [next-state (concat tour [current])]))))

(defn do-transition [graph ants pheromones]
  (let [next-ants (map (fn [ant] (next-step ant graph pheromones)) ants)
        tours (map second next-ants)
        new-pheromones (decay-pheromones pheromones tours)]
    [ants pheromones]))

(defn find-tours [graph ants pheromones]
  (if (every? (comp nil? first) ants)
    [ants pheromones]
    (let [[ants pheromones] (do-transition graph ants pheromones)]
      (recur graph ants pheromones))))

(defn solve [graph antcount init-ants-fn init-pheromones-fn]
  (let [ants (init-ants-fn graph antcount)]
    (loop [time-step 0
           best-tour nil
           pheromones (init-pheromones-fn graph)]
      (if (> time-step 100)
        (list best-tour pheromones) ; return
        (let [[new-ants new-pheromones] (find-tours graph ants pheromones)]
          (recur (inc time-step) (apply (partial min-key
                                                 (partial tour-cost graph))
                                        (map second new-ants))
                 (update-pheromones new-pheromones best-tour)))))))

; p^k[i,j](t)
; i : node
; j : node
; unvisited : set of node
; pheromones : (node node) -> int
; sight : (node node) -> int
; returns : probability of choosing node j as the next node
(defn as-transition-rule [i j unvisited pheromones sight alpha beta]
  (/ (* (expt (pheromones i j) alpha)
        (expt (sight i j) beta))
        (apply + (map (fn [unvisited-node] (* (expt (pheromones i unvisited-node) alpha)
                                              (expt (sight i unvisited-node) beta)))
                      unvisited))))

(defn acs-transition-rule [i j unvisited pheromones sight beta]
  (as-transition-rule i j unvisited pheromones sight 1 beta))

(defn -main [args]
  (let [cities (file->graph (file (first args)))
        antcount (second args)]
    (solve cities antcount (take antcount (shuffle (nodes cities))) #(reduce (partial assoc) {} (edges %) (repeat (/ 1 (* (count (nodes %))
                                                                                                                         (nearest-neighbor-heuristic %)))))

(defn change-pheromones [best-tour tours pheromones]
  (add-pheromone-on-tour (? best-tour)
                         (? best-tour)
                         (decay-pheromone-on-tour (? tours) pheromones))
