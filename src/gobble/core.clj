(ns gobble.core)

(def all-pins 10)
(def frame-size 3)
(def empty-score "")
(def num-of-frames 10)
(def max-frame-size 4)

(defn new-scorecard
  "Create new empty score card."
  [] [])

(defn to-balls "Map frame scores to the raw score of each ball."
  [card]
  (let [balls (->> (rest card)
                   (partition 2 frame-size)
                   (flatten)
                   (filter #(not= "" %)))]
    (->> (count balls)
         range
         (map #(let [current (nth balls %)]
                (cond (= \X current) all-pins
                      (= \/ current) (- all-pins (nth balls (dec %)))
                      :else current))))))

(defn resolve-frame-score
  "Resolve a frames score"
  [[fscore b1 b2 & rest-of-card]]
  (if (and (number? b1) (number? b2))
    (+ b1 b2)                                                                               ;open frame
    (let [next-balls (to-balls (partition 2 frame-size (rest rest-of-card)))
          num-of-next-balls (count next-balls)]
      (cond (and (= b1 \X) (>= num-of-next-balls 2)) (apply + all-pins (take 2 next-balls)) ;strike
            (and (= b2 \/) (>= num-of-next-balls 1)) (+ all-pins (first next-balls))        ;spare
            :else ""))))                                                                    ;cannot be resolved yet, need more balls

(defn resolve-card-score
  "Resolve blank scores as much as possible."
  [card]
  (let [card-size (count card)]
    (->> (- card-size frame-size)
         (iterate #(- % frame-size))
         (take-while #(and (>= % 0) ((complement number?) (nth card %))))
         (map #(list % (resolve-frame-score (drop % card))))
         (reduce (fn [new-card [score-pos score]]
                   (update-in new-card [score-pos] #(identity %2) score))
                 card)
         )))

(defn is-valid? [card & balls] true)                        ;TO DO


(defn on-last-frame?
  "Evaluates if we have all the frames already."
  [card] (= (quot (count card) frame-size) num-of-frames))


(defn score-frame
  "Calculate score for frame. When the operation is valid, returns an updated scorecard.
   Returns nil otherwise. "
  ([card & balls] (let [new-card (resolve-card-score (into card (apply list empty-score balls)))]
                        (if (on-last-frame? new-card)
                          (->> (drop-last new-card)         ;take out the fill-ball entry
                               (partition 1 frame-size)
                               (reduce +)
                               (into new-card))
                          new-card)))
  )
