(ns aco-tsp.graph-test
  (:require [clojure.test :refer :all]
            [aco-tsp.graph :refer :all])
  (:use [clojure.java.io :only [file]]
        [loom.graph :only [weighted-graph]]))

(deftest file->graph-test
         (testing "that given a file, we return a graph"
                  (is (= (file->graph (file "./resources/four.tsp"))
                         (weighted-graph { 0 { 1 61, 2 49, 3 30 },
                                          1 { 2 48, 3 16 },
                                          2 { 3 54 }})))))

(let [short-graph (weighted-graph { 0 { 1 50 }})
      four-city-graph (file->graph (file "./resources/four.tsp"))]
  (deftest nearest-neighbor-heuristic-test
           (testing "a short graph, calculates it right"
                    (is (= (nearest-neighbor-heuristic short-graph)
                           100)))
           (testing "the four city graph"
                    (is (= (nearest-neighbor-heuristic four-city-graph)
                           143)))))
