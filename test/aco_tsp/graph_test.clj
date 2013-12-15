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
