(ns looping-is-recursion)

(defn power [base exp]
  (let [power-helper (fn [acc cnt]
                       (cond
                        (zero? cnt) 1
                        (= 1 cnt) acc
                        :else (recur (* acc base) (dec cnt))))]
    (power-helper base exp)))

(defn last-element [a-seq]
  (if (<= (count a-seq) 1)
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (if (not= (count seq1)
            (count seq2))
    false 
   (let [seq=helper (fn [eq seq1 seq2]
                      (if (or (not eq)
                              (empty? seq1))
                        eq
                        (let [f1 (first seq1) f2 (first seq2)]
                          (recur (= f1 f2) (rest seq1) (rest seq2)))))]
     (seq=helper true seq1 seq2))))

(defn find-first-index [pred a-seq]
  (loop [index 0
         s a-seq]
    (cond
     (empty? s)       nil
     (pred (first s)) index
     :else            (recur (inc index) (rest s)))))

(defn avg [a-seq]
  (if (empty? a-seq) 0
   (loop [s   a-seq
          sum 0
          cnt 0]
     (if (empty? s)
       (/ sum cnt)
       (recur (rest s) (+ sum (first s)) (inc cnt))))))

(defn parity [a-seq]
  (if (empty? a-seq)
    #{}
    (let [toggle (fn [s n]
                   (if (contains? s n)
                     (disj s n)
                     (conj s n)))]
     (loop [a-set #{}
            tail a-seq]
       (if (empty? tail)
         a-set
         (recur (toggle a-set (first tail)) (rest tail)))))))

(defn fast-fibo [n]
  (if (= n 0)
    0
    (loop [fibo   1
           fibo-1 0
           cnt    1]
     (if (= n cnt)
       fibo
       (recur (+ fibo fibo-1) fibo (inc cnt))))))


(defn cut-at-repetition [a-seq]
  (loop [result []
         a-set #{}
         tail a-seq]
    (let [f (first tail)
          r (rest tail)]
     (if (or (empty? tail)
             (contains? a-set f))
       result
       (recur (conj result f) (conj a-set f) r)))))

