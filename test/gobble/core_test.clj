(ns gobble.core-test
  (:require [clojure.test :refer :all]
            [gobble.core :refer :all]))

(deftest test-new-scorecard
  (testing "Test new scorecard."
    (is (= (new-scorecard)  {:frames [] :per-frame-scores [] :running-total [] :final-total "" }))))

(def empty-card {:frames [] :per-frame-scores [] :running-total [] :final-total "" })
(def one-frame-card (update-in empty-card [:frames] #(conj %1 %2) '(2 3)))

(def score-frames (partial reduce #(apply score-frame %1 %2) (new-scorecard)))

(def caf #"Cannot add frame")
(def inb #"Invalid number of balls")
(def ibs #"Invalid ball score")
(def tsi #"Total score is more")

(deftest test-invalid-frames
  (testing "Verify that appropriate exception is thrown for invalid input."
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

(deftest test-score-card
  (testing "Test happy paths on the score card."
    (is (= (new-scorecard)
           {:frames [] :per-frame-scores [] :running-total [] :final-total "" })
        "Wrong empty scorecard.")

    (is (= {:frames ['(4 5)]
            :per-frame-scores [9]
            :running-total [9]
            :final-total "" }
           (score-frame empty-card 4 5))
        "Wrong card when adding open frame to empty card.")
    (is (= {:frames ['(2 3) '(4 5)]
            :per-frame-scores [5 9]
            :running-total [5 14]
            :final-total "" }
           (score-frame one-frame-card 4 5))
        "Wrong card when adding open frame to card with 1 frame.")
    (is (= {:frames ['(4 \/)]
            :per-frame-scores []
            :running-total []
            :final-total "" }
           (score-frame empty-card 4 \/))
        "Wrong card when adding first spare.")
    (is (= {:frames ['(4 \/) '(4 5)]
            :per-frame-scores [14 9]
            :running-total [14 23]
            :final-total "" }
           (score-frames ['(4 \/) '(4 5)]))
        "Wrong card when adding open after first spare.")
    (is (= {:frames (into [] (repeat 3 '(\X "")))
            :per-frame-scores [30]
            :running-total [30]
            :final-total "" }
           (reduce #(apply score-frame %1 %2) empty-card (into [] (repeat 3 '(\X "")))))
        "Wrong card for turkey in the beginning.")
    (is (= {:frames (into [] (concat (repeat 9 '(\X "")) '((\X \X \X))))
            :per-frame-scores (into [] (repeat 10 30))
            :running-total (into [] (take 10 (iterate #(+ 30 %) 30)))
            :final-total 300 }
           (score-frames (concat (repeat 9 '(\X "")) '((\X \X \X)))))
        "Wrong card for perfect game.")
    (is (= {:frames (into [] (concat (repeat 9 '(0 \/)) '((0 \/ 0))))
            :per-frame-scores (into [] (repeat 10 10))
            :running-total (into [] (take 10 (iterate #(+ 10 %) 10)))
            :final-total 100 }
           (score-frames (concat (repeat 9 '(0 \/)) '((0 \/ 0)))))
        "Wrong card for gutter spares.")
    (is (= {:frames (into [] (concat (repeat 9 '(9 \/)) '((9 \/ 9))))
            :per-frame-scores (into [] (repeat 10 19))
            :running-total (into [] (take 10 (iterate #(+ 19 %) 19)))
            :final-total 190 }
           (score-frames (concat (repeat 9 '(9 \/)) '((9 \/ 9)))))
        "Wrong card for gutter spares.")
    (is (= {:frames (into [] (concat (repeat 9 '(9 \/)) '((9 \/ \X))))
            :per-frame-scores (into [] (concat (repeat 9 19) '(20)))
            :running-total (into [] (concat (take 9 (iterate #(+ 19 %) 19)) '(191)))
            :final-total 191 }
           (score-frames (concat (repeat 9 '(9 \/)) '((9 \/ \X)))))
        "Wrong card for gutter spares and strike on last roll.")
    (is (= {:frames (into [] (repeat 10 '(5 4)))
            :per-frame-scores (into [] (repeat 10 9))
            :running-total (into [] (take 10 (iterate #(+ 9 %) 9)))
            :final-total 90 }
           (score-frames (repeat 10 '(5 4))))
        "Wrong card for stubborn pin.")
    (is (= {:frames ['(0 9) '(0 \/) '(\X)' (0 9) '(0 \/) '(\X) '(0 9) '(0 \/) '(\X) '(\X \X \X)]
            :per-frame-scores [9 20 19 9 20 19 9 20 30 30]
            :running-total [9 29 48 57 77 96 105 125 155 185]
            :final-total 185 }
           (score-frames ['(0 9) '(0 \/) '(\X)' (0 9) '(0 \/) '(\X) '(0 9) '(0 \/) '(\X) '(\X \X \X)]))
        "Wrong card for beginners luck.")
    ))

(deftest test-to-rolls
  (testing "test transforming frames to rolls"
    (is (= [all-pins] (to-rolls '(\X ))))
    (is (= [5 5] (to-rolls '(5 \/ ))))))

