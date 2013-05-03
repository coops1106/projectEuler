
(defn collatz-seq [n] (seq (cons n (lazy-seq(cond (= n 1) '() (= (mod n 2) 0) (collatz-seq (/ n 2)) (= (mod n 2) 1) (collatz-seq(+ (* 3 n) 1)))))))

(defn collatz-count-terms [n] (count (collatz-seq n)))

(defn collatz-seq-compare [x y] (cond (>= (collatz-count-terms x) (collatz-count-terms y)) x :else y))

;;(defn prob14 [] (iterate #(collatz-seq-compare % (collatz-count-terms 1)), range(1 1000000)))

(def testn (map first (iterate (fn [[a b]] [(collatz-seq-compare a b) (inc b)]) [1 1])))
