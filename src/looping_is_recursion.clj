(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc exp] (if (zero? exp) acc (recur (* acc base) 
                                                     (dec exp))))] 
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [helper (fn [a-seq n] (if (== n 1) (first a-seq) (recur (rest a-seq) 
                               (dec n))))] 
    (if (empty? a-seq) nil (helper a-seq (count a-seq)))))

(defn seq= [seq1 seq2]
  (let [helper (fn [n seq1 seq2] (if (zero? n) true 
                                   (if (== (first seq1) (first seq2)) 
                                      (recur (dec n) (rest seq1) (rest seq2)) 
                                      false)))] 
      (cond (and (empty? seq1) (empty? seq2)) true
	 (== (count seq1) (count seq2)) (helper (count seq1) seq1 seq2)
         :else false)))

(defn find-first-index [pred a-seq]
  (loop [index 0
         pred pred
         a-seq a-seq] (cond (empty? a-seq) nil
                        (pred (first a-seq)) index
                        :else (recur (inc index) pred (rest a-seq)))))

(defn avg [a-seq]
  (loop [sum 0
         n 0
         a-seq a-seq] (if (empty? a-seq) (if (not= n 0) (/ sum n) 0) 
                        (recur (+ sum (first a-seq)) (inc n) (rest a-seq)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (loop [output (set [])
         a-seq a-seq] (if (empty? a-seq) output 
                        (recur (toggle output (first a-seq)) (rest a-seq)))))

(defn fast-fibo [n]
  (loop [n n
         count 0
         f_n 0
         f_n-1 0] (if (== n count) (+ f_n f_n-1) 
                    (cond (== count 0) (recur n (inc count) (inc f_n) 0)
                      (== count 1) (recur n (inc count) f_n f_n-1)
                      :else (recur n (inc count) (+ f_n f_n-1) f_n)))))

(defn cut-at-repetition [a-seq]
  (loop [output (set [])
         a-seq a-seq] (if (empty? a-seq) (into [] output) 
                        (recur (conj output (first a-seq)) (rest a-seq)))
))

