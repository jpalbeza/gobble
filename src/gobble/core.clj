(ns gobble.core)

(def all-pins 10)
(def empty-score "")
(def num-of-frames 10)

(defn throw-full-card-msg []
  (throw (RuntimeException. "Cannot add frame to a full score card.")))

(defn throw-invalid-ball-count-msg []
  (throw (RuntimeException. "Invalid number of balls. 1-2 balls for non-last frame. 2-3 balls for last frame.")))

(defn throw-invalid-ball-score-msg []
  (throw (RuntimeException. "Invalid ball score. When ball scores are joined, it must match either of these regex: #\"[1-9]{2}|X|[1-9]/\"")))

(defn throw-invalid-ball-score-range-msg []
  (throw (RuntimeException. "Total score is more than available pins.")))

(defn is-valid
  ([frame-pos] (throw-invalid-ball-count-msg))
  ([frame-pos b1] (cond (>= frame-pos num-of-frames) (throw-invalid-ball-count-msg)
                        ((complement =) b1 \X) (throw-invalid-ball-score-msg)))
  ([frame-pos b1 b2] (cond (>= frame-pos num-of-frames)
                           (throw-full-card-msg)  ;cannot add more frames to a full set

                           (= num-of-frames (inc frame-pos))
                           (is-valid frame-pos b1 b2 empty-score) ;validate as last frame

                           :else (let [balls-str (clojure.string/join "" [b1 b2])]
                                   (cond  (not (re-matches #"[1-9]{2}|X|[1-9]/" balls-str))
                                          (throw-invalid-ball-score-msg)

                                          (and (re-matches #"[1-9]{2}" balls-str) (<= all-pins (+ b1 b2)))
                                          (throw-invalid-ball-score-range-msg)))))


  ([frame-pos b1 b2 b3] (cond (>= frame-pos num-of-frames)
                              (throw-full-card-msg)             ;cannot add more frames to a full set

                              ((complement =) num-of-frames (inc frame-pos))
                              (throw-invalid-ball-count-msg)    ;3 elements are only allowed in last frame

                              :else (let [balls-str (clojure.string/join "" [b1 b2 b3])]
                                      (cond  (not (re-matches #"[1-9]{2}|XXX|[1-9]/[1-9]|[1-9]/X" balls-str))
                                             (throw-invalid-ball-score-msg)

                                             (and (re-matches #"[1-9]{2}" balls-str) (<= all-pins (+ b1 b2)))
                                             (throw-invalid-ball-score-range-msg)))))
  ([frame-pos b1 b2 b3 b4 & more] (throw-invalid-ball-count-msg)))

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