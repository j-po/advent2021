(ns syntax
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn ingest-input [file]
  (into [] (map #(s/split % #"")) (line-seq (io/reader (io/resource file)))))

(def braces {"[" "]"
             "(" ")"
             "{" "}"
             "<" ">"})

(def err-scores {")" 3
                 "]" 57
                 "}" 1197
                 ">" 25137})

(def completion-scores {")" 1
                        "]" 2
                        "}" 3
                        ">" 4})

(def ends (into #{} (vals braces)))

(defn parse-line [line]
  (loop [[head & tail] line
         stack []]
    (cond
      (nil? head) (if (empty? stack) [:fine]
                    [:missing (->> stack
                                   (map braces)
                                   reverse)])
      (contains? braces head) (recur tail (conj stack head))
      (= (braces (peek stack)) head) (recur tail (pop stack))
      :else [:corrupt head])))

(defn score-errors [input]
  (->> input
       (map parse-line)
       (filter #(= :corrupt (first %)))
       (map last)
       (map err-scores)
       (apply +)))

(defn score-completions [input]
  (let [scores (->> input
                    (map parse-line)
                    (filter #(= :missing (first %)))
                    (map last)
                    (map #(->> %
                               (map completion-scores)
                               (reduce (fn [x y] (+ y (* 5 x)))))))]
    (nth (sort scores) (quot (count scores) 2))))
