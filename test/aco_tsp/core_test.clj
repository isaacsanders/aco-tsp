(ns aco-tsp.core-test
  (:require [clojure.test :refer :all]
            [aco-tsp.core :refer :all])
  (:use [clojure.java.io :only [file]]
	[aco-tsp.graph :only [file->graph]]))

(deftest init-pheromones-test
	 (testing "creation of a pheromone mapping"
		  (is (= (init-pheromones (file->graph (file "./resources/four.tsp")))
		      {[0 1] 0,
		       [0 2] 0,
		       [0 3] 0,
		       [1 0] 0,
		       [1 2] 0,
		       [1 3] 0,
		       [2 0] 0,
		       [2 1] 0,
		       [2 3] 0,
		       [3 0] 0,
		       [3 1] 0,
		       [3 2] 0}))))

(deftest add-pheromone-test
	 (testing "Adding pheromone to an edge"
	 	  (is (= (add-pheromone 5 
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
			#{2 3} 7}))))

(deftest decay-one-pheromone-test
	 (testing "Decay pheromone on an edge"
	 	  (is (= (decay-one-pheromone (fn [a] (- a 5))
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
			#{2 3} 7}))))

(deftest decay-pheromones-test
	 (testing "Decay pheromones on all edges"
	 	  (is (= (decay-pheromones (fn [a] (- a 5))
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
			#{2 3} 2 }))))

(deftest tour-edges-test
  (testing "Tour edges"
	   (is (= (tour-edges (list 1 2 3 4 1))
		  (list [1 2]
			[2 3]
			[3 4]
			[4 1])))))