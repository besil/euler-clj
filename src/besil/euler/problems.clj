(ns besil.euler.problems
  (:gen-class)
  (:require
   [besil.euler.utils :as utils]))

(defn p1
  ([] (p1 10))
  ([limit]
   (->> (range 1 limit)
        (filter #(or (zero? (mod % 3)) (zero? (mod % 5))))
        (reduce +))))

(defn p2 []
  (->> (utils/fib)
       (take-while #(< % 4000000))
       (filter even?)
       (reduce +)))

(defn p3
  ([] (p3 600851475143))
  ([n]
   (->> (utils/prime-factors n)
        (apply max))))


