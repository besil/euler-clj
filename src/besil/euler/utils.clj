(ns besil.euler.utils)


(defn fib
  [] (map first (iterate (fn [[a b]] [b (+' a b)]) [0 1])))

(defn prime? [n]
  (.isProbablePrime (biginteger n) 16))

(defn primes []
  (cons 2 (filter prime? (iterate #(+ % 2) 3))))

(defn primes-until [n]
  (take-while #(< % n) (primes)))

(defn factorize [n]
  (loop [n n
         d 2
         acc []]
    (cond

      (= n 1)
      acc

      (> (* d d) n)
      (conj acc [n 1])

      (zero? (mod n d))
      (let [[n' e] (loop [m n, e 0]
                     (if (zero? (mod m d))
                       (recur (quot m d) (inc e))
                       [m e]))]
        (recur n' (if (= d 2) 3 (+ d 2)) (conj acc [d e])))

      :else
      (recur n (if (= d 2) 3 (+ d 2)) acc))))

(defn prime-factors [n]
  (map first (factorize n)))

(defn gcd
  ([a] a)
  ([a b] (if (zero? b) a (recur b (mod a b))))
  ([a b & more] (reduce gcd (gcd a b) more)))

(defn lcm
  ([a] a)
  ([a b] (quot (*' a b) (gcd a b)))
  ([a b & more] (reduce lcm (lcm a b) more)))

