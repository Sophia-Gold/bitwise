(ns bitwise-test
  (:require [clojure.test :refer :all]
            [bitwise.core :refer :all]))

(deftest inv-sqrt
  ;; precision varies from Java Math past two decimals
  (is (= (/ (double (Math/floor (* 100.0 (inverse-sqrt 0.15625)))) 100.0)
         (/ (double (Math/floor (* 100.0 (/ 1.0 (Math/sqrt 0.15625))))) 100.0)))
  (is (= (/ (double (Math/floor (* 100.0 (inverse-sqrt 2.0)))) 100.0)
         (/ (double (Math/floor (* 100.0 (/ 1.0 (Math/sqrt 2.0))))) 100.0)))
  (is (= (/ (double (Math/floor (* 100.0 (inverse-sqrt (Math/E))))) 100.0)
         (/ (double (Math/floor (* 100.0 (/ 1.0 (Math/sqrt (Math/E)))))) 100.0))))

(deftest adder
  (is (= (Long/parseLong (apply str (ripple-carry-adder (to-binary-seq 10) (to-binary-seq 10))) 2)
         (+ 10 10)))
  (is (= (Long/parseLong (apply str (ripple-carry-adder (to-binary-seq 50) (to-binary-seq 50))) 2)
         (+ 50 50)))
  (is (= (Long/parseLong (apply str (ripple-carry-adder (to-binary-seq 32) (to-binary-seq 38))) 2)
         (+ 32 38)))
  (is (= (Long/parseLong (apply str (ripple-carry-adder (to-binary-seq 130) (to-binary-seq 250))) 2)
         (+ 130 250))))

(deftest toffoli-table
  (is (= (toffoli 0 0 0) '(0 0 0)))
  (is (= (toffoli 0 0 1) '(0 0 1)))
  (is (= (toffoli 0 1 0) '(0 1 0)))
  (is (= (toffoli 0 1 1) '(0 1 1)))
  (is (= (toffoli 1 0 0) '(1 0 0)))
  (is (= (toffoli 1 0 1) '(1 0 1)))
  (is (= (toffoli 1 1 0) '(1 1 1)))
  (is (= (toffoli 1 1 1) '(1 1 0))))

(deftest fredkin-table
  (is (= (fredkin 0 0 0) '(0 0 0)))
  (is (= (fredkin 0 0 1) '(0 0 1)))
  (is (= (fredkin 0 1 0) '(0 1 0)))
  (is (= (fredkin 0 1 1) '(0 1 1)))
  (is (= (fredkin 1 0 0) '(1 0 0)))
  (is (= (fredkin 1 0 1) '(1 1 0)))
  (is (= (fredkin 1 1 0) '(1 0 1)))
  (is (= (fredkin 1 1 1) '(1 1 1))))
