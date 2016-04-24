(ns gobble.core-test
  (:require [clojure.test :refer :all]
            [gobble.core :refer :all]))

(deftest test-new-scorecard
  (testing "New scorecard"
    (is (= (new-scorecard)  {:frames [], :per-frame-score [], :running-total [], :final-total "" }))))

(def empty-card {:frames [], :per-frame-score [], :running-total [], :final-total "" })
(def one-frame-card (update-in empty-card [:frames] #(conj %1 %2) '(2 3)))
(def nine-frame-card (update-in empty-card [:frames] #(into %1 %2) (repeat 9 '(2 3))))

(deftest test-open-frame-on-empty
  (testing "open frame score on empty card"
    (let [updated-card (score-frame empty-card 4 5)]
      (is (= ['(4 5)] (get-in updated-card [:frames]))))))

(deftest test-open-frame-on-empty-check-score
  (testing "open frame score on empty card, check per-frame-score"
    (let [updated-card (score-frame empty-card 4 5)]
      (is (= {} updated-card)))))

(deftest test-open-frame-on-non-empty-checkscore
  (testing "open frame score on empty card, check per-frame-score"
    (let [updated-card (score-frame one-frame-card 4 5)]
      (is (= {} updated-card)))))

(deftest test-open-frame-on-non-empty
  (testing "open frame score on a card with 1 open frame"
    (is (= ['(2 3) '(4 5)] (get-in (score-frame one-frame-card 4 5) [:frames])))))

(deftest test-almost-full-scorecard
  (testing "open frame score on a card with 1 open frame"
    (is (= (into [] (concat (repeat 9 '(2 3)) '((6 7 8)))) (get-in (score-frame nine-frame-card 6 7 8) [:frames])))))

(deftest test-to-rolls
  (testing "test transforming frames to rolls"
    (is (and (= [all-pins] (to-rolls '("" \X )))
             (= [5 5] (to-rolls '("" 5 \/ )))))))

;(deftest test-last-open-frame
;  (testing "last open frame"
;    (is (= (into nine-frame-card [3 1 2 30]) (score-frame nine-frame-card 1 2)))))

