(ns euler-clj.problems
  (:require
   [clojure.tools.logging :as log]
   [clojure.math.numeric-tower :as math]
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

(defn p4
  "Problem 4: Largest palindrome product"
  []
  (log/info "Problem 4")
  (let [products (for [i (range 100 999)
                       j (range 100 999)]
                   (* i j))
        palindrome? (fn [x] (= (seq (str x)) (reverse (str x))))
        palindromes (filter palindrome? products)
        max-palindrome (reduce max palindromes)]
    (log/info "Result:" max-palindrome)
    max-palindrome))

(defn p5
  "Problem 5: smallest multiple"
  []
  (log/info "Problem 5")
  (reduce math/lcm (range 1 21)))

(defn p6
  "Problem 6: Sum Square Difference"
  [& args]
  (let [n (if (seq args) (first args) 100)
        sum-square (reduce + (map #(* % %) (range 1 (+ n 1))))
        square-sum (math/expt (reduce + (range 1 (+ n 1))) 2)
        difference (- square-sum sum-square)
        ]
    (log/info "Sum square" sum-square)
    (log/info "Square sum" square-sum)
    (log/info "Difference" difference)
    difference
    )
  )
