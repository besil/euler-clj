(ns euler-clj.problems
  (:require
   [clojure.tools.logging :as log]
   [euler-clj.utils :as utils]))

(defn p1
  "Problem 1: Multiples of 3 and 5"
  [& args]
  (let [n (if (seq args) (first args) 1000)]
    (log/info "Problem 1. N:" n)
    (let [result (->>
                  (range n)
                  (filter (fn [x] (or (= (mod x 3) 0) (= (mod x 5) 0))))
                  (reduce +))]
      (log/info "Returning:" result)
      result)))

(defn p2
  "Problem 2: Fibonacci, sum of even valued"
  [& args]
  (let [n (if (seq args) (first args) 4000000)]
    (log/info "Problem 2. N: " n)
    (let [result (->>
                  (take-while (partial > n) (utils/fib))
                  (filter even?)
                  (reduce +))]
      (log/info "Result" result)
      result)))

(defn p3
  "Problem 3: Largest prime factor"
  [& args]
  (let [n (if (seq args) (first args) 600851475143)]
    (log/info "Problem 3. N: " n)
    (let [result (->>
                  (utils/primes-until (java.lang.Math/sqrt n))
                  (filter (partial utils/dividable? n))
                  (last))]
      (log/info "Result" result)
      result)))