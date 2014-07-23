(ns language.core-test
  (:require [clojure.test :refer :all]
            [language.core :as core]))

(deftest a-test
  (testing "FIXME, I fail."
    (are [in out ](= out (core/eval in))
         '1 1
         '((fn [x] x) 42) 42
         '((fn [x] ((fn [x] x) 42) 43)) 42
         '((fn [x] (x)) ((fn [z] (fn [_] z)) 42)) 42
         '(((fn [x] (fn [x] x)) 42) 43) 43
         '((((fn [x] (fn [y] (fn [x] x))) 42) 43) 44) 44
         )))
