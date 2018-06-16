(ns different.core-test
  (:require [clojure.test :refer :all]
            [different.core :refer :all]))

(deftest polynomials
  (testing "x"
    (is (= (diff 'x 'x) 1)))
  (testing "x^2 + x"
    (is (=
         (set (diff 'x '(+ x (expt x 2))))
         (set '(+ 1 (* 2 (expt x 1)))))))
  (testing "x^3"
    (is (=
         (set (diff 'x '(* x x x)))
         (set '(* 3 (expt x 2)))))))


