(ns bitwise.dilate
  (:require [bitwise.lookup-tables]
            [clojure.core :as cc]
            [primitive-math]))

(primitive-math/use-primitive-operators)
;; (set! *warn-on-reflection* true)
;; (set! *unchecked-math* :warn-on-boxed)
