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

