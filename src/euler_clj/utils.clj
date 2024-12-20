(ns euler-clj.utils
  (:require
   [clojure.tools.logging :as log]))

(defn dividable?
  "Returns true if (mod a b) = 0"
  [a b]
  (zero? (mod a b)))

(defn fib
  "Generate all the infinite Fibonacci numbers"
  ([] (fib 1 1))
  ([a b] (lazy-seq (cons a (fib b (+ a b))))))

(defn prime?
  "Check if a number is prime"
  [n]
  (.isProbablePrime (java.math.BigInteger/valueOf n) 3))

(defn primes
  "Generate an infinite sequence of prime numbers"
  []
  (filter prime? (iterate inc 2)))

(defn primes-until
  "Returns all the primes until n"
  [n]
  (take-while (partial > n) (primes)))
