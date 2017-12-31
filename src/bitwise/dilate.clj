;;  Algorithms from D. S. Wise and R. Raman,
;;  "Converting to and from Dilated Integers,"
;;  in IEEE Transactions on Computers,
;;  vol. 57, no. , pp. 567-573, 2007
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns bitwise.dilate
  (:require [bitwise.lookup-tables :refer :all]
            [clojure.core :as cc]
            [primitive-math]))

(primitive-math/use-primitive-operators)
;; (set! *warn-on-reflection* true)
;; (set! *unchecked-math* :warn-on-boxed)

;; Algorithm 1
(defn ^long dilate2 [^Integer x]
  (bit-or (nth dilate-tab2
               (bit-and 0xFF
                        x))
          (bit-shift-left (nth dilate-tab2
                               (unsigned-bit-shift-right (bit-and 0xFFFF
                                                                  x)
                                                         8))
                          16)))

;; Algorithm 2
(defn ^int undilate2 [^long x]
  (bit-or (nth undilate-tab2 (bit-and 0xFF
                                      (bit-or (unsigned-bit-shift-right x
                                                                        7)
                                              x)))
          (bit-shift-left (nth undilate-tab2
                               (bit-and 0xFF
                                        (unsigned-bit-shift-right (bit-or (unsigned-bit-shift-right x
                                                                                                    7)
                                                                          x)
                                                                  16)))
                          8)))

;; Algorithm 3
(defn ^long dilate3 [^Integer x]
  (bit-and 0x49249249
           (* 0x010101
              (bit-or (bit-shift-left (nth dilate-tab3
                                           (unsigned-bit-shift-right (bit-and 0xFFFF
                                                                              x)
                                                                     8))
                                      24)
                      (nth dilate-tab3 (bit-and 0xFF
                                                x))))))

;; Algorithm 4
(defn ^Integer undilate3 [^long x]
  (let [shift (bit-or (unsigned-bit-shift-right (bit-or (unsigned-bit-shift-right x
                                                                                  8)
                                                        x)
                                                8)
                      x)]
    (bit-and (nth dilate-tab3 (bit-and 0xFFF
                                       shift))
             (bit-shift-left (nth dilate-tab3
                                  (bit-and 0xFF
                                           (unsigned-bit-shift-right shift
                                                                     24)))
                             8))))

;; Algorithm 5
(defn ^long dilate2-shift [^Integer x]
  (let [x0 (long x)
        x1 (bit-and 0x00FF00FF
                    (bit-or x0
                            (bit-shift-left x0 8)))
        x2 (bit-and 0x0F0F0F0F;
                    (bit-or x1
                            (bit-shift-left x1 4)))
        x3 (bit-and 0x33333333
                    (bit-or x2
                            (bit-shift-left x2 2)))
        x4 (bit-and 0x55555555
                    (bit-or x3
                            (bit-shift-left x3 1)))]
    x4))

;; replaced by Algorithm 6
(defn ^Integer undilate2-shift [^long x]
  (let [x1 (bit-and 0x33333333
                    (bit-or x
                            (unsigned-bit-shift-right x 1)))
        x2 (bit-and 0x0F0F0F0F
                    (bit-or x1
                            (unsigned-bit-shift-right x1 2)))
        x3 (bit-and 0x00FF00FF
                    (bit-or x2
                            (unsigned-bit-shift-right x2 4)))
        x4 (bit-and 0x0000FFFF
                    (bit-or x3
                            (unsigned-bit-shift-right x3 8)))]
    (int x4)))

;; Algorithm 6
(defn ^Integer undilate2-mul [^long x]
  (let [x1 (bit-and 0x66666666
                    (* x 3))
        x2 (bit-and 0x78787878
                    (* x1 5)) 
        x3 (bit-and 0x7F807F80
                    (* x2 17))
        x4 (bit-and 0x7FFF8000
                    (* x3 257))]
    (int (unsigned-bit-shift-right x4 15))))

;; Algorithm 8
(defn ^long dilate3-mul [^Integer x]
  (let [x0 (long x)
        x1 (bit-and 0xFF0000FF
                    (* x0 0x10001))
        x2 (bit-and 0x0F00F00F
                    (* x1 0x00101)) 
        x3 (bit-and 0xC30C30C3
                    (* x2 0x00011))
        x4 (bit-and 0x49249249
                    (* x3 0x00005))]
    x4))

;; Algorithm 7
(defn ^Integer undilate3-mul [^long x]
  (let [x1 (bit-and 0x0E070381;
                    (* x 0x0FFC0000))
        x2 (bit-and 0x0FF80001
                    (* x1 0x01041)) 
        x3 (bit-and 0x0FFC0000;
                    (* x2 0x40001))]
    (int (unsigned-bit-shift-right x3 18))))
