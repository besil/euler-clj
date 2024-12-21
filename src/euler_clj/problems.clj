(ns euler-clj.problems
  (:require
   [clojure.math.numeric-tower :as math]
   [clojure.tools.logging :as log]
   [euler-clj.utils :as utils :refer [primes]]))

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
        difference (- square-sum sum-square)]
    difference))

(defn p7
  "Problem 7: nth prime"
  [& args]
  (let [n (if (seq args) (first args) 10001)]
    (last (take n (primes)))))

(defn p8
  "Problem 8: Largest product in Series"
  [& args]
  (let [n (if (seq args) (first args) 4)
        str-big-sequence "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"]
    (log/info "Finding largest product of" n "in sequence" str-big-sequence)
    (let
     [n-sequence (map #(parse-long (str %)) (apply list str-big-sequence))
      sliding-windows (fn [n coll] (when (>= (count coll) n)
                                     (map #(take n (drop % coll)) (range 0 (inc (- (count coll) n))))))
      pairs (map (fn [seq] [seq (reduce * seq)]) (sliding-windows n n-sequence))
      max-pair (apply max-key second pairs)]
      (log/info "Max pair" max-pair)
      (second max-pair))))

(defn p9
  "Problem 9: Special Pythagorean Triplet"
  []
  (let [triplet (first (for [a (range 1 1001)
                             b (range a 1001)
                             :let [c (- 1000 (+ a b))]
                             :when (and (< b c) (= (+ (* a a) (* b b)) (* c c)))]
                         [a b c]))]
    (log/info "Triplet:" triplet)
    (log/info "Product" (reduce * triplet))
    (reduce * triplet)))

(defn p10
  "Problem 10: Summation of Primes"
  [& args]
  (let [n (if (seq args) (first args) 10)]
    (reduce + (take-while (partial >= n) (primes)))))
