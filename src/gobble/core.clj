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
  (let [rolls (->> (remove #(= empty-score %) balls)
                   (replace {\X all-pins}))]
    (->> (range (count rolls))
         (map #(let [current (nth rolls %)]
                (if (= \/ current)
                  (- all-pins (nth rolls (dec %)))
                  current))))))


(defn resolve-frame-score
  "Resolve a frames score"
  [[b1 b2 & rest-of-frames]]
  (if (and (number? b1) (number? b2))
    (+ b1 b2)                                                                               ;open frame
    (let [next-rolls (to-rolls rest-of-frames)
          num-of-next-rolls (count next-rolls)]
      (cond (and (= b1 \X) (>= num-of-next-rolls 2)) (apply + all-pins (take 2 next-rolls)) ;strike
            (and (= b2 \/) (>= num-of-next-rolls 1)) (+ all-pins (first next-rolls))        ;spare
            :else empty-score))))                                                           ;cannot be resolved yet, need more balls



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
                                     (filter number?))]
    (into card {:frames            new-frames
                :per-frame-scores  (into per-frame-scores additional-frame-scores)
                :running-total     (reduce #(conj %1 ((fnil + 0) (last %1) %2) )
                                           running-total
                                           additional-frame-scores)
                :final-total       final-total })))