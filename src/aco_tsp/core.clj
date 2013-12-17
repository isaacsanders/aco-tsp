(ns aco-tsp.core
  (:require [clojure.set]
            [loom.graph :as loom])
  (:use [clojure.math.numeric-tower :only [expt]]
        [aco-tsp.graph]
        [loom.io]
        [clojure.java.io :only [file]]))

(defn init-pheromones [g]
  (reduce (fn [m elem] (assoc m elem 0 ))
          {}
          (loom/edges g)))

(defn add-pheromone [amt edge m]
  (update-in m [edge] (partial + amt)))

(defn tau-sub-0 [g]
  (/ 1.0 (* (count (loom/nodes g))
          (nearest-neighbor-heuristic g))))

(defn decay-pheromones [pheromones g edges rho]
  (reduce (fn [p e]
            (let [new-val (+ (* (- 1.0 rho) (p e))
                             (* rho (tau-sub-0 g)))]
              (assoc (assoc p e new-val) (reverse e) new-val)))
          pheromones edges))

(defn visibility [g i j]
  (/ 1.0 (loom/weight g i j)))

(defn tau-eta [g p i beta j]
  (* (p [i j])
     (expt (visibility g i j) beta)))

(defn choose-next-city [graph pheromones current previous constants]
  (let [beta (constants :beta)
        neighbors (set (loom/successors graph current))
        candidates (disj (clojure.set/difference (set (take (constants :cl)
                                                      (sort-by #(loom/weight graph current %)
                                                               neighbors)))
                                           (set previous))
                         current)
        pheromones-fn (fn [candidate] (tau-eta graph pheromones current beta candidate))
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
                         normalizer (apply + (map (partial tau-eta graph pheromones current beta) candidates))]
                     (if (empty? candidates)
                       probs
                       (recur (assoc probs (/ (tau-eta graph pheromones current beta candidate) normalizer)
                                     candidate)
                              (rest candidates)))))
        probability-fn (fn [candidates]
                         (chance-fn (probs-fn {} candidates)))]
    (cond
      (empty? candidates) (apply (partial min-key (partial loom/weight graph current)) neighbors)
      (<= (rand) (constants :q-sub-0)) (apply (partial max-key pheromones-fn) candidates)
      :else (probability-fn candidates))))

; returns chosen tour
(defn next-step [[current tour] graph pheromones constants]
  (let [unvisited (clojure.set/difference (set (loom/nodes graph)) (set tour))]
    (cond
      (nil? current) [current tour]
      (and (= current (first tour)) (empty? unvisited)) [nil (concat tour [current])]
      :else (let [next-state (choose-next-city graph pheromones current tour constants)]
              [next-state (concat tour [current])]))))

(defn do-transition [graph ants pheromones constants]
  (let [next-ants (map (fn [ant] (next-step ant graph pheromones constants)) ants)
        tours (map last next-ants)
        new-pheromones (decay-pheromones pheromones graph (map #(map first %) (vec (zipmap next-ants ants))) (constants :rho))]
    [next-ants pheromones]))

(defn find-tours [graph ants pheromones constants]
  (println (map first ants))
  (if (every? #(nil? (first %)) ants)
    [ants pheromones]
    (let [[ants pheromones] (do-transition graph ants pheromones constants)]
      (recur graph ants pheromones constants))))

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

(defn solve [graph init-ants-fn init-pheromones-fn constants]
  (let [ants (init-ants-fn graph (constants :antcount))]
    (loop [time-step 0
           best-tour nil
           pheromones (init-pheromones-fn graph)]
      (println "begin time step" time-step)
      (if (> time-step 100)
        (list best-tour pheromones) ; return
        (let [[new-ants new-pheromones] (find-tours graph ants pheromones constants)]
          (recur (inc time-step)
                 (apply (partial min-key (partial tour-cost graph))
                        (map last new-ants))
                 (update-pheromones new-pheromones best-tour (constants :rho))))))))

; p^k[i,j](t)
; i : node
; j : node
; unvisited : set of node
; pheromones : (node node) -> int
; sight : (node node) -> int
; returns : probability of choosing node j as the next node
(defn as-transition-rule [i j unvisited pheromones sight constants]
  (let [alpha (constants :alpha)
	beta (constants :beta)]
    (/ (* (expt (pheromones i j) alpha)
	  (expt (sight i j) beta))
       (apply + (map (fn [unvisited-node] (* (expt (pheromones i unvisited-node) alpha)
					     (expt (sight i unvisited-node) beta)))
		     unvisited)))))

(defn aco-transition-rule [i j unvisited pheromones sight constants]
  (as-transition-rule i j unvisited pheromones sight 1 (constants :beta)))

(defn aco-init-ants-fn [graph antcount]
  (map (fn [i] [i []]) (take antcount (shuffle (loom/nodes graph)))))

(defn aco-init-pheromones-fn [graph]
  (zipmap (loom/edges graph) (repeat (tau-sub-0 graph))))

(defn -main [filename antcount] ;beta rho cl q-sub-0]
  (let [cities (file->graph (file filename))
        antcount (Integer/parseInt antcount)
        cl 10 ;(Integer/parseInt cl)
        rho 0.1 ;(Float/parseFloat rho)
        beta 1 ;(Float/parseFloat beta)
        q-sub-0 0.0] ;(Float/parseFloat q-sub-0)]
    (let [[best-tour pheromones] (solve cities
					aco-init-ants-fn
					aco-init-pheromones-fn
					{:antcount antcount,
					 :beta beta,
					 :rho rho,
					 :cl cl,
					 :q-sub-0 q-sub-0})]
      (println best-tour))))
