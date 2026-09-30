(ns besil.euler.problems
  (:gen-class)
  (:require
   [besil.euler.utils :as utils]
   [clojure.string :as str]))

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

(defn p4
  ([] (p4 1000))
  ([limit]
   (let [palindrome? (fn [n] (= (str n) (apply str (reverse (str n)))))
         palindroms (for [x (range 1 limit)
                          y (range 1 limit)
                          :let [prod (* x y)]
                          :when (palindrome? prod)]
                      prod)]
     (apply max palindroms))))

(defn p5
  ([] (p5 20))
  ([limit]
   (apply utils/lcm (range 1 (+ 1 limit)))))

(defn p6
  ([] (p6 100))
  ([limit]
   (let [numbers (range 1 (+ 1 limit))
         sum-of-squares (->> numbers
                             (map #(* % %))
                             (reduce +))
         square-of-sum (->> numbers
                            (reduce +)
                            (#(* % %)))]
     (println "sum of squares: " sum-of-squares)
     (println "square of sum:  " square-of-sum)
     (- square-of-sum sum-of-squares))))

(defn p7
  ([] (p7 10001))
  ([nth]
   (last (take nth (utils/primes)))))


(defn p8
  ([] (p8 13))
  ([n] (let [big-number "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"
             len (count big-number)
             sequences (for [x (range len)
                             :let [y (+ x n)]
                             :when (<= y len)]
                         (subs big-number x y))
             products (->> sequences
                           (map #(mapv (comp parse-long str) %))
                           (map #(reduce * %)))]
         (apply max products))))


