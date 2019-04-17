(ns lens.core-test
  (:require [clojure.test :refer :all]
            [lens.core :refer :all]))

(deftest cast-ray-test
  (testing "cast-ray fail."
    (is (= (cast-ray [1.0 0.0] [1.0 1.0] [[2.0 0.0] [2.0 2.0]]) [2.0 1.0]))
    (is (= (cast-ray [1.0 0.0] [1.0 1.0] [[2.0 0.0] [4.0 2.0]]) nil))
    ))

(deftest refract-ray-test
  (testing "refract-ray fail."
    (is (= (refract-ray [1.0 1.0] [-1.0 0.0] 1.0) [1.0 1.0]))
    (is (= (refract-ray [(Math/cos (/ Math/PI 4.0)) (Math/sin (/ Math/PI 4.0))]
                        [-1.0 0.0]
                        2.0)
           [(Math/cos (/ Math/PI 8.0)) (Math/sin (/ Math/PI 8.0))]))
    ))

