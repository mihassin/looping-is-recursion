(ns looping-is-recursion)

(defn power [base exp]
  (let [help (fn [acc n k]
		(if (zero? k)
		acc
		(recur (* acc n) n (dec k))))]
	(help 1 base exp)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (loop [a seq1 b seq2]
    (cond
     (and (empty? a) (empty? b)) true
     (or (empty? a) (empty? b)) false
     (== (first a) (first b)) (recur (rest a) (rest b))
     :else false)))

(defn find-first-index [pred a-seq]
  (loop [i 0 pred pred a-seq a-seq]
    (cond
     (empty? a-seq) nil
     (pred (first a-seq)) i
     :else (recur (inc i) pred (rest a-seq)))))

(defn sum [a-seq]
  (loop [acc (first a-seq) a-seq a-seq]
    (if (empty? (rest a-seq))
      acc
      (recur (+ acc (second a-seq)) (rest a-seq)))))

(defn avg [a-seq]
  (/ (sum a-seq) (count a-seq)))

(defn parity [a-seq]
  (loop [a-set #{} a-seq a-seq]
    (cond
     (empty? a-seq) a-set
     (contains? a-set (first a-seq)) (recur (disj a-set (first a-seq)) (rest a-seq))
     :else (recur (conj a-set (first a-seq)) (rest a-seq)))))

(defn fast-fibo [n]
  (loop [f1 0
         f2 1
         n n]
    (cond
     (== 0 n) f1
     (== 1 n) f2
     :else (recur f2 (+ f1 f2) (dec n)))))

(defn cut-at-repetition [a-seq]
  (loop [check [] a-seq a-seq]
    (cond
     (empty? a-seq) check
     (contains? (set check) (first a-seq)) check
     :else (recur (conj check (first a-seq)) (rest a-seq)))))

