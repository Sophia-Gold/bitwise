(ns dilation-test
  (:require [clojure.test :refer :all]
            [bitwise.dilate :refer :all]
            [clojure.core :as cc]))

(deftest dilate-two
  (let [x (rand-int (cc/dec (Math/pow 2 16)))]
    (is (= x (undilate2 (dilate2 x))))
    (is (= x (undilate2-shift (dilate2-shift x))))
    (is (= x (undilate2-mul (dilate2-shift x))))))

;; (deftest dilate-three
;;   (let [x (rand-int (cc/dec (Math/pow 2 8)))]
;;     (is (= x (undilate3 (dilate3 x))))
;;     (is (= x (undilate3-mul (dilate3-mul x))))))
