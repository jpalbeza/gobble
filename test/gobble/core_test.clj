(ns gobble.core-test
  (:require [clojure.test :refer :all]
            [gobble.core :refer :all]))

(deftest test-new-scorecard
  (testing "New scorecard"
    (is (let [card (new-scorecard)]
          (and (empty? card)
               (vector? card))))))

(def empty-card [])
(def one-frame-card [3 1 2])
;(def nine-frame-card (vec (take 27 (cycle [3 1 2]))))

(deftest test-open-frame-on-empty
  (testing "open frame score on empty card"
    (is (= '(9 4 5) (score-frame empty-card 4 5)))))

(deftest test-open-frame-on-non-empty
  (testing "open frame score on a card with 1 open frame"
    (is (= '(3 1 2 3 1 2) (score-frame one-frame-card 1 2)))))

(deftest test-to-rolls
  (testing "test transforming frames to rolls"
    (is (and (= [all-pins] (to-rolls '("" \X )))
             (= [5 5] (to-rolls '("" 5 \/ )))))))

;(deftest test-last-open-frame
;  (testing "last open frame"
;    (is (= (into nine-frame-card [3 1 2 30]) (score-frame nine-frame-card 1 2)))))

