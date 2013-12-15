(ns aco-tsp.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn file->graph [f]
  f)

(defn (init-pheromones [g]
		       (reduce (fn [m elem] (assoc m elem 0 ))
			       {}
			       (edges g))))

(defn (add-pheromone [amt edge m]
		     (update-in m [edge] (partial + amt))))

(defn (decay-one-pheromone [decay-fn edge m]
			   (update-in m [edge] (comp (partial max 0) decay-fn))))

(defn (decay-pheromones [decay-fn m]
			(update-in m (keys m) (comp (partial max 0) decay-fn))))