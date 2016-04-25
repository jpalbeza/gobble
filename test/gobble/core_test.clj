(ns gobble.core-test
  (:require [clojure.test :refer :all]
            [gobble.core :refer :all]))

(deftest test-new-scorecard
  (testing "New scorecard"
    (is (= (new-scorecard)  {:frames [] :per-frame-scores [] :running-total [] :final-total "" }))))

(def empty-card {:frames [] :per-frame-scores [] :running-total [] :final-total "" })
(def one-frame-card (update-in empty-card [:frames] #(conj %1 %2) '(2 3)))

(def score-frames (partial reduce #(apply score-frame %1 %2) (new-scorecard)))

(def caf #"Cannot add frame")
(def inb #"Invalid number of balls")
(def ibs #"Invalid ball score")
(def tsi #"Total score is more")

(deftest test-invalid-frames
  (testing "Verify that appropriate exception is thrown for invalid input"
    (is (thrown-with-msg? RuntimeException caf (score-frames (repeat 11 '(1 2)))))
    (is (thrown-with-msg? RuntimeException caf (score-frames (concat (repeat 10 '(1 2)) '((\X \X \X))))))

    (is (thrown-with-msg? RuntimeException inb (score-frames '(()))))
    (is (thrown-with-msg? RuntimeException inb (score-frames '((1 2 3)))))
    (is (thrown-with-msg? RuntimeException inb (score-frames (concat (repeat 9 '(1 2)) '((\X \X \X \X))))))

    (is (thrown-with-msg? RuntimeException ibs (score-frames '((\/)))))
    (is (thrown-with-msg? RuntimeException ibs (score-frames '((5)))))
    (is (thrown-with-msg? RuntimeException ibs (score-frames '((5 \X)))))
    (is (thrown-with-msg? RuntimeException ibs (score-frames '((\X \X)))))
    (is (thrown-with-msg? RuntimeException ibs (score-frames '((\/ 5)))))
    (is (thrown-with-msg? RuntimeException ibs (score-frames '((10 \/)))))

    (is (thrown-with-msg? RuntimeException tsi (score-frames '((5 5)))))))

(deftest test-open-frame-on-empty
  (testing "open frame score on empty card"
    (is (= {:frames ['(4 5)]
            :per-frame-scores [9]
            :running-total [9]
            :final-total "" }
           (score-frame empty-card 4 5))
        "something happened")))

(deftest test-almost-full-scorecard
  (testing "open frame score on a card with 1 open frame"
    (is (= {:frames ['(2 3) '(4 5)]
            :per-frame-scores [5 9]
            :running-total [5 14]
            :final-total "" }
           (score-frame one-frame-card 4 5)))))

(deftest test-first-spare
  (testing "open frame score on a card with 1 open frame"
    (is (= {:frames ['(4 \/)]
            :per-frame-scores []
            :running-total []
            :final-total "" }
           (score-frame empty-card 4 \/)))))

(deftest test-first-spare-followed-by-open
  (testing "open frame score on a card with 1 open frame"
    (is (= {:frames ['(4 \/) '(4 5)]
            :per-frame-scores [14 9]
            :running-total [14 23]
            :final-total "" }
            (reduce #(apply score-frame %1 %2) empty-card ['(4 \/) '(4 5)]) ))))


(deftest test-opening-turkey
  (testing "three strikes in the beginning"
    (is (= {:frames (into [] (repeat 3 '(\X "")))
            :per-frame-scores [30]
            :running-total [30]
            :final-total "" }
           (reduce #(apply score-frame %1 %2) empty-card (into [] (repeat 3 '(\X ""))))))))

(deftest test-perfect-game
  (testing "perfect game"
    (is (= {:frames (into [] (concat (repeat 9 '(\X "")) '((\X \X \X))))
            :per-frame-scores (into [] (repeat 10 30))
            :running-total (into [] (take 10 (iterate #(+ 30 %) 30)))
            :final-total 300 }
           (reduce #(apply score-frame %1 %2) empty-card (concat (repeat 9 '(\X "")) '((\X \X \X))))))))

(deftest test-to-rolls
  (testing "test transforming frames to rolls"
    (is (= [all-pins] (to-rolls '(\X ))))
    (is (= [5 5] (to-rolls '(5 \/ ))))))

