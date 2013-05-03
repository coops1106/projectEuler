(ns user
  (:use [clojure.contrib.lazy-seqs]))

(defn hello-world [name] (str "hello" name))

(defn multiple-of-3-or-5
  "Returns n if n is a multiple of 3 or 5"
  [n]
  (cond (==(mod n 3)0) n (==(mod n 5)0) n :else 0)
)

(defn prob1
  "Attempts to solve project euler problem number 1"
  []
  (reduce + (map multiple-of-3-or-5 (range 1000)))
)

(defn stack-consuming-fibo
  [n]
  (cond(= n 0) 0 (= n 1) 1 :else(+ (stack-consuming-fibo(- n 1))(stack-consuming-fibo (- n 2)))))

(defn tail-fibo [n]
  (letfn [(fib
            [current next n]
            (if (zero? n)
              current
              (fib next (+ current next) (dec n))))]
    (fib 0 1 n)))

(defn lazy-seq-fibo
  ([]
    (concat [0 1] (lazy-seq-fibo 0 1)))
  ([a b]
    (let [n (+ a b)]
      (lazy-seq
        (cons n (lazy-seq-fibo b n))))))

(defn fibo []
  (map first (iterate (fn [[a b]] [b (+ a b)]) [1 2])))

(defn fib [a b] (cons a (lazy-seq(fib b(+ b a)))))

(defn prob2
  "Attempts to solve project Euler problem 2" []
  (reduce + (filter even? (take-while #(< % 4000000) (fibo)))))

(defn filter-test []
  (filter #(< % 5702888) (fib 1 2)))

(defn sum-of-odd-squares [coll]
  (reduce + (map #(* % %)(filter odd? coll))))

(defn square [n] (* n n))

(defn diff-sum-of-sq-sq-of-sum [n] (- (square (reduce + (range (inc n)))) (reduce + (map #(square %) (range (inc n))))))

(defn prob6 [] (diff-sum-of-sq-sq-of-sum 100))

(defn sieve [s]
  (cons (first s)
    (lazy-seq (sieve (filter #(not= 0 (mod % (first s)))(rest s))))))


(defn is-prime [n] (time(every? #(> % 0) (map #(mod n %) (range 2 n)))))

(defn prime? [n] (not-any? zero? (map #(rem n %) (range 2 n))))

(defn factors-of [n] (filter #(zero? (rem n %)) (range 1 (inc n))))

(defn prob3 [] (apply max(take-while prime? (factors-of 600851475143))))

(defn prob7 [] (nth (sieve (iterate inc 2)) 10001))

;;(defn collatz-seq [n] (seq (cons n (lazy-seq(cond (= n 1) '() (= (mod n 2) 0) (collatz-seq (/ n 2)) (= (mod n 2) 1) (collatz-seq(+ (* 3 n) 1)))))))

;;(defn collatz-count [n] (count (collatz-seq n)))


