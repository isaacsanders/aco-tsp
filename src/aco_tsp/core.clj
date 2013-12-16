(ns aco-tsp.core
  (:require [clojure.set])
  (:use [loom.graph]
        [clojure.math.numeric-tower :only [expt]]
        [aco-tsp.graph]
        [clojure.java.io :only [file]]))

(defn init-pheromones [g]
  (reduce (fn [m elem] (assoc m elem 0 ))
          {}
          (edges g)))

(defn add-pheromone [amt edge m]
  (update-in m [edge] (partial + amt)))

(defn visibility [g i j]
  (/ 1.0 (weight g i j)))

(defn tau-eta [g p i j beta]
  (* (p i j)
     (expt (visibility g i j) beta)))

(defn choose-next-city [graph pheromones current previous beta cl q-sub-0]
  (let [candidates (take cl (sort-by #(weight graph current %)
                                     (clojure.set/difference (set (successors graph current))
                                                             (set previous))))
        pheromones-fn (fn [candidate] (tau-eta graph pheromones current candidate beta))
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
                         normalizer (float (apply + (map (partial tau-eta graph pheromones current beta) candidates)))]
                     (if (empty? candidates)
                       probs
                       (recur (assoc probs (/ (tau-eta graph pheromones current candidate beta) normalizer)
                                     candidate)
                              (rest candidates)))))
        probability-fn (fn [candidates]
                         (chance-fn (probs-fn {} candidates)))]
    (if (empty? candidates)
      (apply (partial max-key (partial weight graph current)) (successors graph current))
      (if (<= (rand) q-sub-0)
        (apply (partial max-key pheromones-fn) candidates)
        (probability-fn candidates)))))

; returns chosen tour
(defn next-step [[current tour] graph pheromones beta cl q-sub-0]
  (let [unvisited (clojure.set/difference (set (nodes graph)) (set tour))]
    (case
      (nil? current) [current tour]
      (and (= current (first tour)) (empty? unvisited)) [nil (concat tour [current])]
      :else (let [next-state (choose-next-city graph pheromones current tour beta cl q-sub-0)]
              [next-state (concat tour [current])]))))

(defn do-transition [graph ants pheromones beta cl q-sub-0]
  (let [next-ants (map (fn [ant] (next-step ant graph pheromones beta cl q-sub-0)) ants)
        tours (map second next-ants)
        new-pheromones (decay-pheromones pheromones (vec (zipmap (filter (comp not nil?) (map first ants))
                                                            (filter (comp not nil?) (map first next-ants)))))]
    [ants pheromones]))

(defn find-tours [graph ants pheromones beta cl q-sub-0]
  (if (every? #(nil? (first %)) ants)
    [ants pheromones]
    (let [[ants pheromones] (do-transition graph ants pheromones beta cl q-sub-0)]
      (recur graph ants pheromones))))

(defn tour-edges [node-list]
  (second (reduce (fn [[prev edges-list] node]
                    (cond
                      (nil? prev) [node edges-list]
                      :else [node (concat edges-list (list [prev node]))]))
                      [nil (list)] node-list)))

(defn update-pheromones [pheromones tour rho]
  (reduce (fn [p edge] (assoc p edge (+ (* (- 1 rho) (pheromones edge))
                                        (* rho (/ 1 (tour-cost tour))))))
          pheromones (tour-edges tour)))

(defn solve [graph antcount init-ants-fn init-pheromones-fn beta rho cl q-sub-0]
  (let [ants (init-ants-fn graph antcount)]
    (loop [time-step 0
           best-tour nil
           pheromones (init-pheromones-fn graph)]
      (if (> time-step 100)
        (list best-tour pheromones) ; return
        (let [[new-ants new-pheromones] (find-tours graph ants pheromones beta cl q-sub-0)]
          (recur (inc time-step) (apply (partial min-key
                                                 (partial tour-cost graph))
                                        (map second new-ants))
                 (update-pheromones new-pheromones best-tour rho)))))))

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

(defn aco-transition-rule [i j unvisited pheromones sight beta]
  (as-transition-rule i j unvisited pheromones sight 1 beta))

(defn aco-init-ants-fn [graph antcount]
  (map (fn [i] [i []]) (take antcount (shuffle (nodes graph)))))

(defn aco-init-pheromones-fn [graph]
  (zipmap (edges graph) (repeat (/ 1 (* (count (nodes graph))
					(nearest-neighbor-heuristic graph))))))

(defn -main [filename antcount beta rho cl q-sub-0]
  (let [cities (file->graph (file filename))
        antcount (Integer/parseInt antcount)]
    (let [[best-tour pheromones] (solve cities antcount
					aco-init-ants-fn
					aco-init-pheromones-fn
					beta
					rho
					cl
					q-sub-0)]
      )))

;(defn change-pheromones [best-tour tours pheromones]
;  (add-pheromone-on-tour (? best-tour)
;                         (? best-tour)
;                         (decay-pheromone-on-tour (? tours) pheromones))
