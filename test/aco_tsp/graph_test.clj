(ns aco-tsp.graph-test
  (:require [clojure.test :refer :all]
            [aco-tsp.graph :refer :all])
  (:use [clojure.java.io :only [file]]))

(deftest file->graph-test
         (testing "that given a file, we return a graph"
                  (is (file->graph (file "./resources/four.tsp"))
                      { 0 { 1 61, 2 49, 3 30 },
                        1 { 0 61, 2 48, 3 16 },
                        2 { 0 49, 1 48, 3 54 },
                        3 { 0 30, 1 16, 2 54 } })))
                            ;(fn [cost-fn a b]
                            ;  (let [cost-fn ]
                            ;    (cost-fn (sort (vec a b))))))

(deftest init-pheromones-test
	 (testing "creation of a pheromone mapping"
	 	  (is (init-pheromones (file->graph (file "./resources/four.tsp)))
		      { #{0 1} 0,
		      	#{0 2} 0,
			#{0 3} 0,
			#{1 2} 0,
			#{1 3} 0,
			#{2 3} 0})))

(deftest add-pheromone-test
	 (testing "Adding pheromone to an edge"
	 	  (is (add-pheromone 5 
		      		     #{0, 1} 
				     { #{0 1} 2,
				       #{0 2} 3,
				       #{0 3} 4,
				       #{1 2} 5,
				       #{1 3} 6,
				       #{2 3} 7})
		      { #{0 1} 7,
		      	#{0 2} 3,
			#{0 3} 4,
			#{1 2} 5,
			#{1 3} 6,
			#{2 3} 7})))

(deftest decay-one-pheromone-test
	 (testing "Decay pheromone on an edge"
	 	  (is (decay-one-pheromone (fn [a] (- a 5))
		      		     	   #{0, 1} 
				     	   { #{0 1} 2,
				     	     #{0 2} 3,
				    	     #{0 3} 4,
				    	     #{1 2} 5,
				             #{1 3} 6,
				       	     #{2 3} 7})
		      { #{0 1} 0,
		      	#{0 2} 3,
			#{0 3} 4,
			#{1 2} 5,
			#{1 3} 6,
			#{2 3} 7})))

(deftest decay-pheromones-test
	 (testing "Decay pheromones on all edges"
	 	  (is (decay-pheromones (fn [a] (- a 5))
				     	{ #{0 1} 2,
				       	  #{0 2} 3,
				       	  #{0 3} 4,
				       	  #{1 2} 5,
				       	  #{1 3} 6,
				       	  #{2 3} 7})
		      { #{0 1} 0,
		      	#{0 2} 0,
			#{0 3} 0,
			#{1 2} 0,
			#{1 3} 1,
			#{2 3} 2})))