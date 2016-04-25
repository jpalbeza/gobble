(ns gobble.core)

(def all-pins 10)
(def frame-size 3)
(def empty-score "")
(def num-of-frames 10)


(defn new-scorecard "Create new empty score card."
  [] {:frames [] :per-frame-scores [] :running-total [] :final-total empty-score })


(defn to-rolls "Map frame scores to the raw score of each ball."
  [balls]
  (let [rolls (replace {\X all-pins} balls)]
    (->> (range (count rolls))
         (map #(let [current (nth rolls %)]
                (if (= \/ current)
                  (- all-pins (nth rolls (dec %)))
                  current))))))


(defn resolve-frame-score "Resolve a frames score"
  [[b1 & rest-of-frames]]
  (let [rolls-after-b1 (remove #(= empty-score %) rest-of-frames) ;eliminate empty-scores as they are not considered as rolls
        b2 (first rolls-after-b1)
        rolls-after-b2 (rest rolls-after-b1)]
    (cond (and (= b1 \X) (>= (count rolls-after-b1) 2)) (apply + all-pins (take 2 (to-rolls rolls-after-b1))) ;strike
          (and (= b2 \/) (>= (count rolls-after-b2) 1)) (+ all-pins (first (to-rolls rolls-after-b2)))        ;spare
          (and (number? b1) (number? b2)) (+ b1 b2)                                                           ;open frame
          :else empty-score)))                              ;cannot be resolved yet, need more rolls


(def full-card-msg "Cannot add frame to a full score card.")
(defn throw-full-card-exception [] (throw (RuntimeException. full-card-msg)))

(def invalid-number-of-balls-msg "Invalid number of balls. 1-2 balls for non-last frame. 1-3 balls for last frame.")
(defn throw-invalid-number-of-balls [] (throw (RuntimeException. invalid-number-of-balls-msg)))

(defn is-valid
  ([frame-pos] (throw-invalid-number-of-balls))
  ([frame-pos b1 b2] (cond (>= frame-pos num-of-frames) (throw-full-card-exception)           ;cannot add more frames to a full set
                           (= num-of-frames (inc frame-pos)) (is-valid frame-pos b1 b2 "") ;validate as last frame
                           :else (or (and (number? b1) (or (number? b2) (= b2 \/))) ;open frame or spare
                                     (and (= b1 \X) (= b2 empty-score)))))                   ;strike
  ([frame-pos b1 b2 b3] (cond (>= frame-pos num-of-frames)  (throw-full-card-exception)                   ;cannot add more frames to a full set
                              ((complement =) num-of-frames (inc frame-pos)) (throw-invalid-number-of-balls) ;3 elements are only allowed in last frame
                              :else (or (and (number? b1) (number? b2) (= empty-score b3))             ;open frame on last frame
                                        (and (number? b1) (and (= b2 \/) (or (number? b3) (= b3 \X)))) ;spare on last frame, completed by either a number or a strike
                                        (= (repeat 3 \X) (list b1 b2 b3)))))                          ;turkey on last frame
  ([frame-pos b1 b2 b3 b4 & more] (throw-invalid-number-of-balls))
  )


(defn score-frame
  "Calculate score for frame. When the operation is valid, returns an updated scorecard.
   Returns nil otherwise. "
  [{:keys [frames per-frame-scores running-total final-total] :as card}
   & balls]
  (let [new-frames (conj frames balls)
        additional-frame-scores (->> (range (count per-frame-scores) (count new-frames))
                                     (map #((comp resolve-frame-score flatten drop) % new-frames))
                                     (filter number?))
        new-running-total (reduce #(conj %1 ((fnil + 0) (last %1) %2) )
                                  running-total
                                  additional-frame-scores)]
    (apply is-valid (count frames) balls)
    (into card {:frames            new-frames
                :per-frame-scores  (into per-frame-scores additional-frame-scores)
                :running-total     new-running-total
                :final-total       (if (= num-of-frames (count new-frames))
                                     (last new-running-total)
                                     final-total)})))