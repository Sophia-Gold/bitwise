(ns bitwise.core)

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

(defn xor-swap [a b]
  (let [a (bit-xor a b)
        b (bit-xor b a)
        a (bit-xor a b)]
    (list a b)))

(defn to-binary-seq [^long x]
  (map #(- (int %) (int \0))
       (Long/toBinaryString x)))

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

;; (defn bitwise-reverse [x]
;;   (with-local-vars [i x]
;;     (var-set i (bit-or
;;                 (bit-shift-right (bit-and x 0xaaaaaaaa) 1)
;;                 (bit-shift-left (bit-and x 0x55555555) 1)))
;;     (var-set i (bit-or
;;                 (bit-shift-right (bit-and x 0xcccccccc) 2)
;;                 (bit-shift-left (bit-and x 0x33333333) 2)))
;;     (var-set i (bit-or
;;                 (bit-shift-right (bit-and x 0xf0f0f0f0) 4)
;;                 (bit-shift-left (bit-and x 0x0f0f0f0f) 4)))
;;     (var-set i (bit-or
;;                 (bit-shift-right (bit-and x 0xff00ff00) 8)
;;                 (bit-shift-left (bit-and x 0x00ff00ff) 8)))
;;     (var-set i (bit-or
;;                 (bit-shift-right (bit-and x 0xffff0000) 16)
;;                 (bit-shift-left (bit-and x 0x0000ffff) 16)))
;;     @i))

;; (defn quicksort [lst]
;;   (let [t-lst (transient (into [] lst))]
;;     (loop [pivot 0]
;;       (when-not (empty? (nthnext t-lst (inc pivot)))
;;         (if (> (nth t-lst pivot) (nth t-lst (inc pivot)))
;;           (xor-swap (nth t-lst pivot) (nth t-lst (inc pivot))))
;;         (recur (inc pivot))))
;;     (persistent! t-lst)))

(defn to-lower [^String a]
  (apply str
         (map
          (comp char #(bit-xor % 0x20) byte int) a)))

(defn to-upper [^String a]
  (apply str
         (map (comp char #(bit-xor 0x20 %) byte int) a)))

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

(defn partition-string [n string]
  (let [length (count string)]
    (loop [i 0
           result '()]
      (if (= i n)
        (reverse result)
        (recur (inc i) (cons (subs string (* (/ length n) i) (* (/ length n) (inc i))) result))))))

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
        
(defn half-adder [a b]
  [(bit-xor a b)
   (bit-and a b)])

(defn full-adder [a b carry]
  (let [c (half-adder b carry)
        d (first c)]
    [(first (half-adder a d))
     (bit-or (second (half-adder a d)) (second c))]))

(defn ripple-carry-adder [a b]
  (with-local-vars [a-binary (to-binary-seq a), b-binary (to-binary-seq b)]
    (let [a-length (count @a-binary)
          b-length (count @b-binary)]
      (cond
        (< a-length b-length) (var-set a-binary (concat (repeat (- b-length a-length) 0) @a-binary))
        (> a-length b-length) (var-set b-binary (concat (repeat (- a-length b-length) 0) @b-binary)))
      (loop [sum '()
             a-rev (reverse @a-binary)
             b-rev (reverse @b-binary)
             carry 0]
        (let [added (full-adder (first a-rev) (first b-rev) carry)]
          (if (and (nil? (next a-rev)) (nil? (next b-rev)))
            (cons (first added) sum)
            (recur (cons (first added) sum) (next a-rev) (next b-rev) (second added))))))))

(defn -main []
  )
