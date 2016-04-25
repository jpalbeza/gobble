(ns gobble.core)

(def all-pins 10)
(def frame-size 3)
(def empty-score "")
(def num-of-frames 10)


(defn new-scorecard
  "Create new empty score card."
  [] {:frames [] :per-frame-scores [] :running-total [] :final-total empty-score })


(defn to-rolls "Map frame scores to the raw score of each ball."
  [balls]
  (let [rolls (replace {\X all-pins} balls)]
    (->> (range (count rolls))
         (map #(let [current (nth rolls %)]
                (if (= \/ current)
                  (- all-pins (nth rolls (dec %)))
                  current))))))


(defn resolve-frame-score
  "Resolve a frames score"
  [[b1 & rest-of-frames]]
  (let [rolls-after-b1 (remove #(= empty-score %) rest-of-frames) ;eliminate empty-scores as they are not considered as rolls
        b2 (first rolls-after-b1)
        rolls-after-b2 (rest rolls-after-b1)]
    (cond (and (= b1 \X) (>= (count rolls-after-b1) 2)) (apply + all-pins (take 2 (to-rolls rolls-after-b1))) ;strike
          (and (= b2 \/) (>= (count rolls-after-b2) 1)) (+ all-pins (first (to-rolls rolls-after-b2)))        ;spare
          (and (number? b1) (number? b2)) (+ b1 b2)                                                           ;open frame
          :else empty-score)))                              ;cannot be resolved yet, need more rolls



(defn is-valid? [card & balls] true)                        ;TO DO


(defn on-last-frame?                                        ;TO DO
  "Evaluates if we have all the frames already."
  [card] (= (quot (count card) frame-size) num-of-frames))


(defn score-frame
  "Calculate score for frame. When the operation is valid, returns an updated scorecard.
   Returns nil otherwise. "
  [{:keys [frames per-frame-scores running-total final-total] :as card}
   & balls]
  (let [new-frames (conj frames balls)
        additional-frame-scores (->> (range (count per-frame-scores) (count new-frames))
                                     (map #((comp resolve-frame-score flatten drop) % new-frames))
                                     (filter number?)
                                     )]
    (into card {:frames            new-frames
                :per-frame-scores  (into per-frame-scores additional-frame-scores)
                :running-total     (reduce #(conj %1 ((fnil + 0) (last %1) %2) )
                                           running-total
                                           additional-frame-scores)
                :final-total       final-total
                })))