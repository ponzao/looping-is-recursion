(ns looping-is-recursion)

(defn power
  [base exp]
  (loop [acc 1 i 0]
    (if (>= i exp)
      acc 
      (recur (* acc base) (inc i)))))

(defn last-element
  [a-seq]
  (loop [acc nil coll a-seq]
    (if (seq coll)
      (recur (first coll) (rest coll))
      acc)))

(defn seq=
  [seq1 seq2]
  (loop [c1 seq1 c2 seq2]
    (cond (and (empty? c1) (empty? c2)) true
          (or (empty? c1) (empty? c2)) false
          (= (first c1) (first c2)) (recur (rest c1) (rest c2))
          :else false)))

(defn find-first-index
  [pred a-seq]
  (loop [i 0 coll a-seq]
    (when-let [s (seq coll)]
      (if (pred (first s))
        i
        (recur (inc i) (rest coll))))))

(defn avg
  [a-seq]
  (loop [coll a-seq sum 0 n 0]
    (if (empty? coll)
      (/ sum n)
      (recur (rest coll)
             (+ (first coll) sum)
             (inc n)))))

(defn- toggle
  [a-set elem]
  (if (a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity
  [a-seq]
  (loop [acc #{} coll a-seq]
    (if (empty? coll)
      acc
      (recur (toggle acc (first coll)) (rest coll)))))

(defn fast-fibo
  [n]
  (loop [i 0 x 0 y 1]
    (if (< i n)
      (recur (inc i) (+ x y) x)
      x)))

(defn cut-at-repetition
  [a-seq]
  (loop [acc [] coll a-seq seen #{}]
    (cond (empty? coll) acc
          (seen (first coll)) acc
          :else (recur (conj acc (first coll))
                       (rest coll)
                       (conj seen (first coll))))))
      

