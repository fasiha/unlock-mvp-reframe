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

(defn drop-nth-vec
  ([v n] (drop-nth-vec v n 1))
  ([v n how-many] (into (subvec v 0 n) (subvec v (+ n how-many) (count v)))))

(defn replace-subvec [v start how-many to-replace]
  (let [[left right] (split-at start v)
        right (drop how-many right)]
    (concat left (take how-many to-replace) right)))

