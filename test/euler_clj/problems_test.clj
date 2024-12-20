(ns euler-clj.problems-test
  (:require
   [clojure.test :refer :all]
   [euler-clj.core :refer :all]
   [euler-clj.problems :as problems]))

(deftest ^:latest p1-test
  (is (= 23
         (problems/p1 10))))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 1 1))))

(deftest a-2nd-test
  (testing "FIXME, I fail."
    (is (= 1 1))))