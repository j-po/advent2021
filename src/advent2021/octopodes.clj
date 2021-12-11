(ns octopodes
  (:require [clojure.core.match :refer [match]]
            [clojure.java.io :as io]
            [clojure.string :as s]))

;; not technically a "depth" because the `(loop ... (recur ...))`
;; construct in Clojure is meant to emulate tail call elimination
(def MAX_RECURSION_DEPTH (Long/parseLong "DEADBEEF" 16))


(defn ingest-input [file]
  (into []
        (map
          (fn [line]
            (into []
                  (map #(Integer/parseInt %))
                  (s/split line #""))))
        (line-seq (io/reader (io/resource file)))))

(defn bump
  "increment the value at each [row column] position given in pts in
  the given matrix. return a tuple of the new matrix and all of the
  positions whose values this operation caused to exceed 9"
  [matrix pts]
  (reduce (fn [[m candidates] [r c]]
            (let [new-val (inc (get-in m [r c]))
                  candidates* (if (> new-val 9)
                                (conj candidates [r c])
                                candidates)]
              [(assoc-in m [r c] new-val) candidates*]))
          [matrix []]
          pts))

(defn game
  "a state machine for the Conway's-Game-of-Life-like game
  described in the problem"
  [input]
  (let [rows (count input)
        cols (count (first input))
        all (for [r (range rows)
                  c (range cols)]
              [r c])]
    (letfn [(neighbors [[r c]]
              (filter (fn [[y x]]
                        (and (<= 0 y (dec rows))
                             (<= 0 x (dec cols))))
                      [[(inc r) c]
                       [(inc r) (inc c)]
                       [(inc r) (dec c)]
                       [(dec r) c]
                       [(dec r) (inc c)]
                       [(dec r) (dec c)]
                       [r (inc c)]
                       [r (dec c)]]))]
      (loop [step 1
             op [:promote]
             octopodes input
             flashed #{} 
             flashes 0]
        ;; I'm sure I could have made better use of pattern-matching
        ;; here, but this worked fine.
        (match op

               [:promote]
               ;; Increment all octopodes' energy levels.
               (let [[octopodes* candidates] (bump octopodes all)]
                 (recur step
                        [:flash candidates]
                        octopodes*
                        flashed
                        flashes))

               [:flash candidates]
               ;; Increment energy levels for all neighbors of any
               ;; octopode whose energy level > 9 and that hasn't
               ;; "flashed" yet this step, and repeat until no more
               ;; octopodes can flash this step.
               (if-let [to-flash (seq (set (filter #(not (flashed %))
                                                   candidates)))]
                 (let [[octopodes* candidates*] (bump octopodes
                                                      (mapcat neighbors
                                                              to-flash))]
                   (recur step
                          [:flash candidates*]
                          octopodes*
                          (into flashed to-flash)
                          (+ flashes (count to-flash))))
                 ;; If no more octopodes can flash this step, zero out
                 ;; the ones that did and move on.
                 (recur step
                        [:wrap-up]
                        (reduce (fn [m [r c]] (assoc-in m [r c] 0))
                                octopodes
                                flashed)
                        flashed
                        flashes))

               [:wrap-up]
               ;; Terminate if the termination criteria have been met,
               ;; otherwise go on to the next step.
               (if (or (= step MAX_RECURSION_DEPTH)
                       (every? zero? (apply concat octopodes)))
                 {:step step
                  :octopodes octopodes}
                 (recur (inc step)
                        [:promote]
                        octopodes
                        #{}
                        flashes)))))))
