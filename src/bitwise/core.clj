(ns bitwise.core
  (:require [clojure.core :as cc]
            [primitive-math]))

(primitive-math/use-primitive-operators)
;; (set! *warn-on-reflection* true)
;; (set! *unchecked-math* :warn-on-boxed)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; NUMERICS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn gcd [^long a ^long b]
  (cond
    (zero? a) b
    (zero? b) a
    (> 0 a) (- a)
    (> 0 b) (- b)
    (and (even? a) (even? b)) (* 2
                                 (long (gcd (unsigned-bit-shift-right a 1)
                                            (unsigned-bit-shift-right b 1))))
    (and (even? a) (odd? b)) (recur (unsigned-bit-shift-right a 1) b)
    (and (odd? a) (even? b)) (recur a (unsigned-bit-shift-right b 1))
    (and (odd? a) (odd? b)) (recur (unsigned-bit-shift-right
                                    (Math/abs (long (- a b))) ;; coerce to avoid reflection
                                    1) (min a b))))

(defn bit-shift-double [^double x ^long shifts]
  (let [x-long (Double/doubleToRawLongBits x)]
    (Double/longBitsToDouble
     (bit-or (bit-and 1 x-long)
             (bit-shift-left (- (bit-shift-right x-long 52) shifts) 52)
             (bit-and 0xfffffffffffff x-long)))))

(defn inverse-sqrt [^double x]
  (let [y (Double/longBitsToDouble
           (- 0x5FE6EB50C7B537A9  ;; magic constant for doubles (https://cs.uwaterloo.ca/~m32rober/rsqrt.pdf)
              (bit-shift-right (Double/doubleToRawLongBits x) 1)))]
    (* y
       (- 1.5
          (* 0.5 x y y)))))

(defn log10 [^long x]
  (+ 1.0
     (* (double (Integer/numberOfTrailingZeros (bit-shift-right x 1)))
        (double (bit-shift-double 1233 12)))))
  
(defn to-binary-seq [x]
  (map #(- (cc/long %) (cc/long \0))
       (Integer/toBinaryString x)))

(defn long-to-vec [^long i]
  (mapv #(- (long %) 48) (str i)))

(defn bit-count [^long x]
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
  (let [chars (map (comp cc/long char) string)
        len (count string)]
    (reduce +'
            (map #(* (long (Math/pow 31 (- len (long %2)))))
                 chars
                 (range len)))))
  
(defn murmur3 [^String string]
  (let [chars (map (comp long char) string)
        len (* (count chars) 2)]
    (letfn [(mix1 [^long x]
              (* 0x1b873593
                 (Integer/rotateLeft (* 0xcc9e2d51 x) 15)))
            (mix2 [x y]
              (+ 0xe6546b64
                 (* 5
                    (Integer/rotateLeft (bit-xor x y) 13))))
            (avalanche [x]
              (let [xor (bit-xor x len)
                    right-16 (unsigned-bit-shift-right xor 16)
                    xor-16 (bit-xor xor right-16)
                    hex-mul-1 (* xor-16 0x85ebca6b)
                    hex-mul-2  (* (bit-xor hex-mul-1
                                           (unsigned-bit-shift-right hex-mul-1 13))
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
  (let [hash (long (hash-string (name sym)))
        sym-ns (namespace sym)
        seed (hash-string (if (nil? sym-ns) (str *ns*) sym-ns))]
    (bit-xor seed
             (+ hash
                0x9e3779b9
                (bit-shift-left seed 6)
                (bit-shift-right seed 2)))))

(defn rolling-hash [base s]
  (->> s
       (reverse)
       (map-indexed #(* (long (Math/pow base %1)) (long %2)))
       (reduce +' 0)
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

(defn partition-string [^long n string]
  ;; add offset
  (let [length (long (count string))]
    (loop [i 0
           result '()]
      (if (= i n)
        (reverse result)
        (recur (inc i) (cons (subs string (* (/ length n) i) (* (/ length n) (inc i))) result))))))

(defn bloom-conj [base chunk-size string]
  (let [sparse (->> string
                    (rolling-hash base)
                    (str)
                    (map #(- (long %) 48))
                    (#(partition chunk-size %))
                    (map (comp long
                               #(reduce +' %)
                               (fn [coll] (map-indexed #(* (long (Math/pow 10 %1)) (long %2)) coll))))
                    (sort)
                    (dedupe))]
    (->> (map -' (next sparse) sparse)
         (cons (inc (long (first sparse))))
         (mapcat #(concat (repeat (dec (long %)) 0) [1])))))
    
(defn bloom-contains? [base chunk-size bitvec hash]
  (let [hash (->> hash
                   (str)
                   (map #(- (long %) 48))
                   (#(partition chunk-size %))
                   (map (comp long
                              #(reduce +' %)
                              (fn [coll] (map-indexed #(* (long (Math/pow 10 %1)) (long %2)) coll))))
                   (sort)
                   (dedupe))]
    (if (< (count bitvec) (long (apply cc/max hash)))
      false
      (loop [hash hash]
        (cond
          (empty? hash) true
          (zero? (long (nth bitvec (first hash)))) false
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
                               (- (long s-hash)
                                  (* (long (first s)) roll)))
                            (long (nth s p-length)))
                         (inc count)))))))
  clojure.lang.Seqable
  (rabin-karp
    [p ^String s]
    (let [base 128
          chunk-size 2
          p (map #(map long %) p)
          p-length (count (first p))
          bitvec (apply map cc/bit-or (map #(bloom-conj base chunk-size %) p))
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
                               (- (long s-hash)
                                  (* (long (first s)) roll)))
                            (long (nth s p-length)))
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
    (map #(map cc/bit-xor %
               (flatten (take (count (first binary)) (repeat key))))
         binary)))

(defn xor-decrypt [msg key]
  (apply str
         (map (comp char #(Long/parseLong % 2) #(apply str %))
              (map #(map cc/bit-xor (flatten (take (count (first msg)) (repeat key))) %)
                   msg))))

(defn lfsr [s {[^long x ^long y ^long z] :taps}]
  (concat (drop 1 s)
          [(bit-xor (nth (- x 1))
                    (nth s (- y 1))
                    (nth s (- z 1)))]))

(defn p-box []
  )

(defn s-box []
  )

(defn feistel [msg1 msg2 key]
  (let [key-length (count key)
        exp-msg1 (concat msg1 (take (- key-length (count msg1)) msg1))
        exp-msg2 (concat msg2 (take (- key-length (count msg2)) msg2))
        new-msg2 (p-box (s-box (xor-encrypt msg1 key)))]
    [new-msg2 (xor-encrypt new-msg2 msg1) key]))
    
(defn permutations [msg key ^long rounds {:keys [^long offset] :or {offset 0}}]
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

(defn feistel-encrypt [msg key ^long blocks ^long rounds]
  (letfn [(inner-loop [m k ^long i cipher]
            (if (= i blocks)
              cipher
              (recur (next m) (next k) (inc i) (conj cipher (xor-encrypt (first m) (first k))))))
          (outer-loop [m ^long i cipher]
            (if (> i (/ blocks 2))
              cipher
              (recur (pop (next m)) (inc i) (conj cipher (inner-loop (first m) (peek m) 0 [])))))]
    (let [msg-blocks (partition-string blocks msg)
          key-blocks (partition-all (/ (count key) blocks) key)]
      (loop [i 0
             cipher (inner-loop msg-blocks key-blocks 0 [])]
        (if (= i rounds)
          cipher
          (recur (inc i) (outer-loop cipher 0 [])))))))

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
    (let [bit-sum (first (full-adder (first a) (first b) carry))
          bit-carry (long (second (full-adder (first a) (first b) carry)))]
      (if (and (empty? (next a)) (empty? (next b)))
        (conj sum bit-sum (bit-or carry 1))
        (recur (next a) (next b) (conj sum bit-sum) bit-carry)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; LOGIC GATES
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn toffoli [a b c]
  (list a
        b
        (bit-xor (bit-and a b) c)))

(defn fredkin [a b c]
  (let [s (bit-and (bit-xor b c) a)]
    (list a
          (bit-xor b s)
          (bit-xor c s))))

(defn -main []
  )
