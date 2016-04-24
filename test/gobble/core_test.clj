(ns gobble.core-test
  (:require [clojure.test :refer :all]
            [gobble.core :refer :all]))

(deftest test-new-scorecard
  (testing "New scorecard"
    (is (= (new-scorecard)  {:frames [] :per-frame-scores [] :running-total [] :final-total "" }))))

(def empty-card {:frames [], :per-frame-scores [], :running-total [], :final-total "" })
(def one-frame-card (update-in empty-card [:frames] #(conj %1 %2) '(2 3)))
(def nine-frame-card (update-in empty-card [:frames] #(into %1 %2) (repeat 9 '(2 3))))

(deftest test-open-frame-on-empty
  (testing "open frame score on empty card"
    (is (= {:frames ['(4 5)]
            :per-frame-scores [9]
            :running-total [9]
            :final-total "" }
           (score-frame empty-card 4 5)))))

(deftest test-almost-full-scorecard
  (testing "open frame score on a card with 1 open frame"
    (is (= {:frames ['(2 3) '(4 5)]
            :per-frame-scores [5 9]
            :running-total [5 14]
            :final-total "" }
           (score-frame one-frame-card 4 5)))))

(deftest test-to-rolls
  (testing "test transforming frames to rolls"
    (is (= [all-pins] (to-rolls '("" \X ))))
    (is (= [5 5] (to-rolls '("" 5 \/ ))))))

;(deftest test-last-open-frame
;  (testing "last open frame"
;    (is (= (into nine-frame-card [3 1 2 30]) (score-frame nine-frame-card 1 2)))))

