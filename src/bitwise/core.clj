(ns bitwise.core
  (:require [clojure.test :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; NUMERICS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn gcd [a b]
  (cond
    (zero? a) b
    (zero? b) a
    (neg? a) (- a)
    (neg? b) (- b)
    (and (even? a) (even? b)) (* 2
                                 (gcd (unsigned-bit-shift-right a 1)
                                      (unsigned-bit-shift-right b 1)))
    (and (even? a) (odd? b)) (recur (unsigned-bit-shift-right a 1) b)
    (and (odd? a) (even? b)) (recur a (unsigned-bit-shift-right b 1))
    (and (odd? a) (odd? b)) (recur (unsigned-bit-shift-right (Math/abs (- a b)) 1) (min a b))))

(defn bit-shift-double [^double x shifts]
  (let [x-long (Double/doubleToRawLongBits x)]
    (Double/longBitsToDouble
     (bit-or (bit-and 1 x-long)
             (bit-shift-left (- (bit-shift-right x-long 52) shifts) 52)
             (bit-and 0xfffffffffffff x-long)))))

(defn inverse-sqrt [x]
  (let [y (Float/intBitsToFloat
           (- 0x5f3759df
              (bit-shift-right (Float/floatToRawIntBits x) 1)))]
    (* y
       (- 1.5
          (* 0.5 x y y)))))

(defn log10 [x]
  (+ 1
     (* (Integer/numberOfTrailingZeros (bit-shift-right x 1))
        (bit-shift-double 1233 12))))
  
(defn to-binary-seq [^long x]
  (map #(- (int %) (int \0))
       (Long/toBinaryString x)))

(defn long-to-vec [^Long i]
  (mapv (comp #(- % 48) long) (str i)))

(defn bit-count [x]
  (loop [i 0
         v x]
    (if (= v 0)
      i
      (recur (inc i)
             (bit-and v (- v 1))))))

(defn reverse-bits [x]
  (Long/parseLong
   (apply str (reverse (to-binary-seq x)))
   2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; NON-CRYPTOGRAPHIC HASH FUNCTIONS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn hash-string [^String string]
  (let [chars (map (comp long char) string)
        len (count chars)]
    (.longValue
     (reduce +
             (map #(* %1 (Math/pow 31 (- len %2)))
                  chars
                  (range len))))))
  
(defn murmur3 [^String string]
  (let [chars (map (comp long char) string)
        len (* (count chars) 2)]
    (letfn [(mix1 [x]
              (* 0x1b873593
                 (Integer/rotateLeft (.intValue (* 0xcc9e2d51 (.intValue x))) 15)))
            (mix2 [x y]
              (+ 0xe6546b64
                 (* 5
                    (Integer/rotateLeft (.intValue (bit-xor x y)) 13))))
            (avalanche [x]
              (let [xor (bit-xor x len)
                    right-16 (unsigned-bit-shift-right xor 16)
                    xor-16 (.intValue (bit-xor xor right-16))
                    hex-mul-1 (* xor-16 0x85ebca6b)
                    hex-mul-2  (* (.intValue (bit-xor hex-mul-1
                                                      (unsigned-bit-shift-right hex-mul-1 13)))
                                  0xc2b2ae35)]
                (bit-xor hex-mul-2
                         (unsigned-bit-shift-right hex-mul-2 16))))]
      (avalanche
       (reduce #(bit-xor %2 (mix1 %1))
               (map #(mix2 0
                           (mix1 (bit-or %1
                                         (bit-shift-left %2 16))))
                    (take-nth 2 chars)
                    (take-nth 2 (next chars))))))))

(defn hash-symbol [sym]
  (let [hash (hash-string (name sym))
        sym-ns (namespace sym)
        seed (hash-string (if (nil? sym-ns) (str *ns*) sym-ns))]
    (bit-xor
     seed
     (+ hash
        0x9e3779b9
        (bit-shift-left seed 6)
        (bit-shift-right seed 2)))))

(defn rolling-hash [base s]
  (->> s
       (reverse)
       (map-indexed #(* (Math/pow base %1) %2))
       (reduce + 0)
       (#(rem % 9223372036854775807))
       (long)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; STRINGOLOGY
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn to-lower [^String a]
  (apply str
         (map
          (comp char #(bit-xor % 0x20) byte int) a)))

(defn to-upper [^String a]
  (apply str
         (map (comp char #(bit-xor 0x20 %) byte int) a)))

(defn partition-string [n string]
  ;; add offset
  (let [length (count string)]
    (loop [i 0
           result '()]
      (if (= i n)
        (reverse result)
        (recur (inc i) (cons (subs string (* (/ length n) i) (* (/ length n) (inc i))) result))))))

(defn bloom-conj [base chunk-size string]
  (let [sparse (->> string
                    (rolling-hash base)
                    (str)
                    (map (comp #(- % 48) long))
                    (#(partition chunk-size %))
                    (map (comp long
                               #(reduce + %)
                               (fn [coll] (map-indexed #(* (Math/pow 10 %1) %2) coll))))
                    (sort)
                    (dedupe))]
    (->> (map - (next sparse) sparse)
         (cons (inc (first sparse)))
         (mapcat #(concat (repeat (dec %) 0) [1])))))
    
(defn bloom-contains? [base chunk-size bitvec hash]
  (let [hash (->> hash
                   (str)
                   (map (comp #(- % 48) long))
                   (#(partition chunk-size %))
                   (map (comp long
                              #(reduce + %)
                              (fn [coll] (map-indexed #(* (Math/pow 10 %1) %2) coll))))
                   (sort)
                   (dedupe))]
    (if (< (count bitvec) (apply max hash))
      false
      (loop [hash hash]
        (cond
          (empty? hash) true
          (zero? (nth bitvec (first hash))) false
          :else (recur (next hash)))))))
      
(defprotocol Rabin-Karp
  (rabin-karp [s p]))

(extend-protocol Rabin-Karp
  String
  (rabin-karp
    [^String p ^String s]
    (let [base 128
          p (map long p)
          p-length (count p)
          p-hash (rolling-hash base p)
          s (map long s)
          end (- (count s) p-length)
          roll (long (Math/pow base (dec p-length)))]
      (loop [s s
             s-hash (rolling-hash base (take p-length s))
            count 0]
        (let [test (take p-length s)]
          (cond 
            (and (= p-hash s-hash) (= p test)) count
            (= count end) false
            :else (recur (drop 1 s)
                         (+ (* base
                               (- s-hash
                                  (* (first s) roll)))
                            (nth s p-length))
                         (inc count)))))))
  clojure.lang.Seqable
  (rabin-karp
    [p ^String s]
    (let [base 128
          chunk-size 2
          p (map #(map long %) p)
          p-length (count (first p))
          bitvec (apply map bit-or (map #(bloom-conj base chunk-size %) p))
          s (map long s)
          end (- (count s) p-length)
          roll (long (Math/pow base (dec p-length)))]
      (loop [s s
             s-hash (rolling-hash base (take p-length s))
             count 0]
        (let [test (take p-length s)]
          (cond
            (bloom-contains? base
                             chunk-size
                             bitvec
                             s-hash) (let [matches (keep-indexed #(if (true? %2) %1)
                                                                 (map #(= test %) p))]
                                       (if (not (empty? matches))
                                         (list (clojure.string/join (map char (nth p (first matches))))
                                               count)))
            (= count end) false
            :else (recur (drop 1 s)
                         (+ (* base
                               (- s-hash
                                  (* (first s) roll)))
                            (nth s p-length))
                         (inc count))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; CYPHERS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn xor-swap [a b]
  (let [a (bit-xor a b)
        b (bit-xor b a)
        a (bit-xor a b)]
    (list a b)))

(defn random-key [length]
  (take length (repeatedly #(rand-int 2))))

(defn xor-encrypt [msg key]
  (let [binary (map (comp to-binary-seq int) msg)]
    (map #(map bit-xor %
               (flatten (take (count (first binary)) (repeat key))))
         binary)))

(defn xor-decrypt [msg key]
  (apply str
         (map (comp char #(Long/parseLong % 2) #(apply str %))
              (map #(map bit-xor (flatten (take (count (first msg)) (repeat key))) %)
                   msg))))
(defn s-box []
  ;; https://en.wikipedia.org/wiki/S-box
  )

(defn p-box []
  ;; https://en.wikipedia.org/wiki/Permutation_box
  )

(defn lfsr [s {[x y z] :taps}]
  (concat (drop 1 s)
          [(bit-xor (nth s (- x 1))
                   (nth s (- y 1))
                   (nth s (- z 1)))]))

(defn feistel [msg1 msg2 key]
  (let [key-length (count key)
        exp-msg1 (concat msg1 (take (- key-length (count msg1)) msg1))
        exp-msg2 (concat msg2 (take (- key-length (count msg2)) msg2))
        new-msg2 (p-box (s-box (xor-encrypt msg1 key)))]
    [new-msg2 (xor-encrypt new-msg2 msg1) key]))
    
(defn permutations [msg key rounds & {:keys [offset] :or {offset 0}}]
  ;; offset is float between 0-1
  (let [msg-length (/ (count msg) 2)
        key-length (/ (count key) 2)
        msg1 (take (* msg-length offset) msg)
        msg2 (drop (* msg-length (+ offset 1)) msg)
        key1 (take key-length key)
        key2 (drop key-length key)]
    (loop [n 0
           block1 msg1
           block2 msg2
           key key1
           next-key key2]
      (when (< n rounds)
        (let [f (feistel msg1 msg2 key)]
          (recur (inc n) (first f) (second f) (lfsr next-key) (peek f)))))))

(defn feistel-encrypt [msg key blocks rounds]
  (letfn [(inner-loop [m k i cipher]
            (if (= i blocks)
              cipher
              (recur (next m) (next k) (inc i) (conj cipher (xor-encrypt (first m) (first k))))))
          (outer-loop [m i cipher]
            (if (> i (/ blocks 2))
              cipher
              (recur (pop (next m)) (inc i) (conj cipher (inner-loop (first m) (peek m) 0 [])))))]
    (let [msg-blocks (partition-string blocks msg)
          key-blocks (partition-all (/ (count key) blocks) key)]
      (loop [i 0
             cipher (inner-loop msg-blocks key-blocks 0 [])]
        (if (= i rounds)
          cipher
          (recur (outer-loop cipher 0 []) (inc i)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               
;; ADDERS
;;                                                                                   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn half-adder [a b]
  [(bit-xor a b)
   (bit-and a b)])

(defn full-adder [a b carry]
  (let [added (half-adder b carry)
        half-sum (first added)]
    [(first (half-adder a half-sum))
     (bit-or (second (half-adder a half-sum)) (second added))]))

(defn ripple-carry-adder [a b]
  (loop [a (reverse a)
         b (reverse b)
         sum '()
         carry 0]
    (let [added (full-adder (first a) (first b) carry)]
      (if (and (empty? (next a)) (empty? (next b)))
        (conj sum (first added) (bit-or carry 1))
        (recur (next a) (next b) (conj sum (first added)) (second added))))))

(deftest adder
  (is (= (Long/parseLong (apply str (ripple-carry-adder (to-binary-seq 10) (to-binary-seq 10))) 2)
         (+ 10 10)))
  (is (= (Long/parseLong (apply str (ripple-carry-adder (to-binary-seq 50) (to-binary-seq 50))) 2)
         (+ 50 50)))
  (is (= (Long/parseLong (apply str (ripple-carry-adder (to-binary-seq 32) (to-binary-seq 38))) 2)
         (+ 32 38)))
  (is (= (Long/parseLong (apply str (ripple-carry-adder (to-binary-seq 130) (to-binary-seq 250))) 2)
         (+ 130 250))))

(defn -main []
  )
