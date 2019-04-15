(ns lens.core-test
  (:require [clojure.test :refer :all]
            [lens.core :refer :all]))

(deftest cast-ray-test
  (testing "cast-ray fail."
    (is (= (cast-ray [1.0 0.0] [1.0 1.0] [[2.0 0.0] [2.0 2.0]]) [2.0 1.0]))
    ))

