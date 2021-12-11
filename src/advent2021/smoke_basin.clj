(ns smoke-basin
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn ingest-input [file]
  (into []
        (map
          (fn [line]
            (into []
                  (map #(Integer/parseInt %))
                  (s/split line #""))))
        (line-seq (io/reader (io/resource file)))))

(defn num-rows [heightmap]
  (count heightmap))

(defn num-cols [heightmap]
  (count (first heightmap)))

(defn low-point? [heightmap r c]
  (and
    (or (= 0 c) (< (get-in heightmap [r c]) (get-in heightmap [r (dec c)])))
    (or (= 0 r) (< (get-in heightmap [r c]) (get-in heightmap [(dec r) c])))
    (or (= (dec (num-cols heightmap)) c)
        (< (get-in heightmap [r c]) (get-in heightmap [r (inc c)])))
    (or (= (dec (num-rows heightmap)) r)
              (< (get-in heightmap [r c]) (get-in heightmap [(inc r) c])))))

(defn low-points [heightmap]
  (for [r (range (num-rows heightmap))
        c (range (num-cols heightmap))
        :when (low-point? heightmap r c)]
    [r c]))

(defn sum-risk-levels [heightmap]
  (reduce + (map (fn [rc] (inc (get-in heightmap rc))) (low-points heightmap))))

(defn basin-pts [heightmap [r c]]
  (letfn [(candidate? [seen [y x]]
            (not (or (seen [y x])
                     (neg? y)
                     (neg? x)
                     (<= (num-rows heightmap) y)
                     (<= (num-cols heightmap) x)
                     (= 9 (get-in heightmap [y x])))))]
    (loop [[rr cc] [r c]
           seen #{[r c]}
           to-visit clojure.lang.PersistentQueue/EMPTY
           acc [[r c]]]
      (let [neighbors [[(inc rr) cc] [(dec rr) cc] [rr (inc cc)] [rr (dec cc)]]
            new-nodes (filter (partial candidate? seen)
                              neighbors)
            new-seen (into seen neighbors)
            new-queue (into to-visit new-nodes)]
        (if (empty? new-queue)
          acc
          (recur (peek new-queue) new-seen (pop new-queue) (into acc new-nodes)))))))

(defn basins [heightmap]
  (map (partial basin-pts heightmap) (low-points heightmap)))

(defn prod-3-largest [heightmap]
  (->> heightmap
       basins
       (map count)
       sort
       reverse
       (take 3)
       (apply *)))
