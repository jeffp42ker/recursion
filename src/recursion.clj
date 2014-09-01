(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (cond (empty? coll) false
        (empty? (rest coll)) true
        :else false))

(defn my-last [coll]
  (cond (empty? coll) nil
        (singleton? coll) (first coll)
        :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (cond (< (count seq-1) (count seq-2)) seq-2
        :else seq-1))

(defn longest-sequence [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond (empty? a-seq) ()
        (pred? (first a-seq)) (cons (first a-seq)
                                    (my-filter pred? (rest a-seq)))
        :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond (empty? a-seq) false
        (=  elem (first a-seq))  true
        :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond (empty? a-seq) ()
        (pred? (first a-seq)) (cons (first a-seq)
                                    (my-take-while pred? (rest a-seq)))
        :else ()))

(defn my-drop-while [pred? a-seq]
  (cond (empty? a-seq) ()
        (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
        :else a-seq))

(defn seq= [a-seq b-seq]
  (cond (and (empty? a-seq) (empty? b-seq)) true
        (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
        :else false))

(defn my-map [f seq-1 seq-2]
  (cond (or (empty? seq-1) (empty? seq-2)) ()
        (and (seq seq-1) (seq seq-2))
        (cons (f (first seq-1) (first seq-2))
              (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond (= k  0) 1
        :else (* n (power n (dec k)))))

(defn fib [n]
  (cond (= n 0) 0
        (= n 1) 1
        :else (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond (> how-many-times 0)
        (cons what-to-repeat
              (my-repeat (dec how-many-times) what-to-repeat))
        :else ()))

(defn my-range [up-to]
  (cond (= up-to 0) ()
        :else (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (cond (empty? a-seq) '(())
        :else (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (cond ((empty? a-seq) (reverse a-seq))
        :else (cons (seq a-seq)
                    (inits (reverse (rest (reverse a-seq)))))))

(defn rotations-helper-map [a-seq]
  (let [observed-rotations {}]
    (cond (not (contains? observed-rotations a-seq))
          false)))

;; currently broken

(defn rotations [a-seq]
  (cond (empty? a-seq) '(())

        ; base case
        (not (seq? (first a-seq)))
        (let [current-seq (seq a-seq)
              new-seq (concat (rest current-seq) (cons (first current-seq) ()))]
          (rotations (cons new-seq ())))

        ; completed
        (= (count a-seq) (count (first a-seq))) a-seq

        :else
        (let [current-seq (first a-seq)
              new-seq (concat (rest current-seq) (cons (first current-seq) ()))]
          (rotations (cons new-seq a-seq)))))

#_(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
     (let [new-count (if (= elem (first coll))
                      (inc n)
                      n)]
      (count-elem-helper new-count
                         elem
                         (rest coll)))))

#_(defn my-frequencies [a-seq]
  (frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  [:-])

(defn my-take [n coll]
  [:-])

(defn my-drop [n coll]
  [:-])

(defn halve [a-seq]
  [:-])

(defn seq-merge [a-seq b-seq]
  [:-])

(defn merge-sort [a-seq]
  [:-])

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])
