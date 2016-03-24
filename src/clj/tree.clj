(ns mvp-reframe.tree
  (:require [clojure.string :as string]
            [clojure.pprint :as p]))

(defrecord Taggable [morphemes tags children])

(defn squash-wrap [operation sentence idx]
  (if (and (nth sentence (+ 1 idx) false) ; merge idx and (1+idx)th entries
           (or (= operation :squash) (= operation :wrap)))
    (let [[left right] (split-at idx sentence)
          [a b] (take 2 right)
          right (drop 2 right)
          middle (Taggable. (into (:morphemes a) (:morphemes b))
                            []
                            (if (= operation :squash) 
                              [] 
                              [a b]))]
      (apply conj (vec left) middle right))
    ; TODO just return the original input if either/both argument checks fail?
    sentence))

(def squash (partial squash-wrap :squash))
(def wrap (partial squash-wrap :wrap))

(def morphemes ["nippon" "jin" "jya" "nai"])
(def s (mapv #(Taggable. [(morphemes %)] [] []) (range (count morphemes))))
(def s1 (->
          s
          (squash 2)
          (update-in ,,, [2] assoc ,,, :tags ["じゃない"])
          ))
(def s2 (->
          s1
          (update-in ,,, [0] assoc ,,, :tags ["日本"])
          (update-in ,,, [1] assoc ,,, :tags ["人"])))
(def s3 (->
          s2
          (wrap 0)
          (update-in [0] assoc :tags ["日本人"])))

(p/pprint s3)
(p/pprint (-> s3 (squash 0)))
(p/pprint (-> s3 (wrap 0)))


