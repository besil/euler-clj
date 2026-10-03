(ns besil.euler.sequences)

(defn fib
  [] (map first (iterate (fn [[a b]] [b (+' a b)]) [0 1])))

(defn triangle-numbers
  ([] (triangle-numbers 1 1))
  ([acc n]
   (lazy-seq
    (cons acc (triangle-numbers (+ acc (inc n)) (inc n))))))

(defn collatz-numbers [n]
  (lazy-seq
   (cons n
         (when (not= n 1)
           (collatz-numbers (if (even? n)
                              (/ n 2)
                              (+ (* 3 n) 1)))))))
