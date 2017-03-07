(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                  (if (zero? exp)
                    1
                    (if (= exp 1)
                      acc
                      (recur (* acc base) base (dec exp)))))]
    (helper base base exp)))

(defn last-element [a-seq]
  (let [helper (fn [last a-seq]
                  (if (empty? a-seq)
                    last
                    (recur (first a-seq) (rest a-seq))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (loop [seq1 seq1
         seq2 seq2]
    (if (and (empty? seq1) (empty? seq2))
      true
      (if (or (empty? seq1)
              (empty? seq2)
              (not (= (first seq1) (first seq2))))
        false
        (recur (rest seq1) (rest seq2))))))

(defn find-first-index [pred a-seq]
  (loop [i 0
         a a-seq]
    (if (empty? a)
      nil
      (if (pred (first a))
        i
        (recur (inc i) (rest a))))))

(defn avg [a-seq]
  (loop [sum 0
         n 0
         a a-seq]
    (if (empty? a)
      (if (zero? n)
        0
        (/ sum n))
      (recur (+ sum (first a)) (inc n) (rest a)))))

(defn parity [a-seq]
  (let [toggle
        (fn [set elem]
          (if (contains? set elem)
            (disj set elem)
            (conj set elem)))]
    (loop [s #{}
           a a-seq]
      (if (empty? a)
        s
        (recur (toggle s (first a)) (rest a))))))

(defn fast-fibo [n]
  (cond
    (< n 1) 0
    (= 1 n) 1
    :else
      (loop [a 0
             b 1
             i 2]
        (if (= i n)
          (+ a b)
          (recur b (+ a b) (inc i))))))

(defn cut-at-repetition [a-seq]
  (loop [results []
         s #{}
         a a-seq]
    (if (or (empty? a) (contains? s (first a)))
      results
      (recur (conj results (first a)) (conj s (first a)) (rest a)))))
