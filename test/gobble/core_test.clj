(ns gobble.core-test
  (:require [clojure.test :refer :all]
            [gobble.core :refer :all]))

(deftest test-new-scorecard
  (testing "New scorecard"
    (is (let [initial-scorecard (new-scorecard)]
          (and (empty? initial-scorecard)
               (seq? initial-scorecard))))))
