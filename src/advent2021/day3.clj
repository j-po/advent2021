(ns advent2021.day3
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def fetch
  (memoize (fn [filename]
             (line-seq (io/reader (io/resource filename))))))

(defn as-vecs [lines]
  (map #(vec (map (fn [x] (Integer/parseInt x)) (s/split % #""))) lines))

(defn line-vecs->gamma-str [line-vecs]
  (let [columns (apply map vector line-vecs)
        modes (map #(->> %
                         frequencies
                         (sort-by second)
                         last
                         first)
                   columns)]
    (apply str modes)))

(defn gamma->epsilon [gamma len]
  (let [mask (Integer/parseInt (repeat "1" len) 2)]
    (bit-and (bit-not gamma) mask)))

(defn power-consumption-rate [file]
  (let [gamma-str (line-vecs->gamma-str (as-vecs (fetch file)))
        gamma (Integer/parseInt gamma-str 2)
        epsilon (bit-not gamma)]
    (* gamma epsilon)))

(power-consumption-rate "day3")
