(ns mvp-reframe.kana-kanji
  (:require [clojure.string :as string]))

(def hiragana "ぁあぃいぅうぇえぉおかがきぎくぐけげこごさざしじすずせぜそぞただちぢっつづてでとどなにぬねのはばぱひびぴふぶぷへべぺほぼぽまみむめもゃやゅゆょよらりるれろゎわゐゑをんゔゕゖ")
(def katakana     "ァアィイゥウェエォオカガキギクグケゲコゴサザシジスズセゼソゾタダチヂッツヅテデトドナニヌネノハバパヒビピフブプヘベペホボポマミムメモャヤュユョヨラリルレロヮワヰヱヲンヴヵヶ")

(def katakana-to-hiragana-map
  (into {} (map vector
                (string/split katakana #"")
                (string/split hiragana #""))))

(defn katakana-to-hiragana [text]
  (string/join (map #(or (katakana-to-hiragana-map %) %) text)))

(def non-kana-pattern (re-pattern (str "[^" hiragana katakana "]")))
(defn any-kanji? [text] (re-find non-kana-pattern text))

;;; Clj/cljs utilities

; see documentation in taggers.clj
(defn merge-with-vector-values [vector-of-maps] (apply merge-with into {} vector-of-maps))

(defn drop-nth-vec
  ([v n] (drop-nth-vec v n 1))
  ([v n how-many] (into (subvec v 0 n) (subvec v (+ n how-many) (count v)))))

(defn replace-subvec [v start how-many to-replace]
  (let [[left right] (split-at start v)
        right (drop how-many right)]
    (concat left (take how-many to-replace) right)))

(defn dupes
  ([v] (dupes (sort v) '()))
  ([sorted-v dupes] (if-let [head (first sorted-v)]
                      (recur (rest sorted-v)
                             (if (and (= head (second sorted-v))
                                      (not= head (first dupes)))
                               (conj dupes head)
                               dupes))
                      dupes)))

