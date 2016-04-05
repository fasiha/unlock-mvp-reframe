(ns mvp-reframe.taggers
  (:require [clojure.java.io :as io]
            [clojure.walk :as walk]
            [clojure.string :as string]
            [cheshire.core :as json]
            [mvp-reframe.kana-kanji :as kana]))

;; Jmdict-furigana integration

(defn combine-adjacent
  "Process a seq called `arr` with a combiner function to somehow combine
  adjacent elements that are amenable to combination. `combiner` must be a 2-ary
  function that returns a seq.

  Example: a seq contains both numbers and keywords. Adjacent numbers can be
  collapsed via addition, but adjacent keywords, or keyword-number pairs, cannot
  be meaningfully combined:
  ```clj
  (defn combine-numbers [left right]
    (if (and (= (type left) (type right))       ; if both are same type
                                                ; (N.B.: Double not= Long !)
             (instance? java.lang.Number left)) ; & both are numbers,
      [(+ left right)]                          ; then add (& return seq)
      [left right]))                            ; else, return args as seq.
  (combine-adjacent combine-numbers [1 2 :hi :there 3 4 5 :bye])
  ; => (3 :hi :there 12 :bye)
  ```"
  [combiner arr]
  (reduce
    (fn [memo curr]
      (if (empty? memo)
        [curr]
        (let [all-but-prev (drop-last memo)
              prev (last memo)]
          (concat all-but-prev (combiner prev curr)))))
    []
    arr))

(defn update-kanji-strvec-with-reading [kanji-vec [idx-str reading]]
  (let [[start end] (string/split idx-str #"-")
        start (Integer. start)
        how-many (if end
                   (+ 1 (- (Integer. end) start)) ;"1-2" should yield 2
                   1)
        kanji (apply str (subvec kanji-vec start (+ how-many start)))]
    (into
      []
      (kana/replace-subvec
        kanji-vec
        start
        how-many
        (concat [{:kanji kanji :reading reading}] (repeat nil))))))

(def update-kanji-strvec-with-readings (partial reduce update-kanji-strvec-with-reading))

(defn combine-strings [left right]
  (if (and (string? left) (string? right))
    [(str left right)]
    [left right]))

(defn jmdict-furigana-parse-line [s]
  (let [[headword reading furigana-raw] (string/split s #"\|")
        headword-pieces (->> headword seq (mapv str ,,,)) ; ("全" "日" "本")
        furigana-pieces (string/split furigana-raw #";") ; ["0:ぜん" "1-2:にほん"]
        furigana-pieces (map #(string/split % #":") furigana-pieces) ; (["0" "ぜん"] ["1-2" "にほん"])
        ]
    {:headword headword
     :reading reading
     :furigana (->>
                 (update-kanji-strvec-with-readings headword-pieces furigana-pieces)
                 (filter identity ,,,)
                 (combine-adjacent combine-strings ,,,)
                 (mapv #(if (:kanji %) % {:kanji nil :reading %}) ,,,))}))

(def jmdict-furigana-filename "JmdictFurigana.txt")
(def all-furigana
  (->>
    jmdict-furigana-filename
    io/resource
    slurp
    string/trim
    string/split-lines
    distinct
    (mapv jmdict-furigana-parse-line)
    ))
; (t/headword-reading-to-furigana ["人" "じん"])
(def headword-reading-to-furigana
  (->>
    all-furigana
    (mapv (fn [o] [[(:headword o) (:reading o)] o]) ,,,)
    (into {} ,,,)))

;; JMdict proper

(def jmdict-filename "JMdict-full.ldjson")

(def all-entries
  (->>
    jmdict-filename
    io/resource
    slurp
    string/trim
    string/split-lines
    (mapv (comp walk/keywordize-keys json/parse-string))
    ))

;; Lookup routines

; To see what the following does, consider:
; ```clj
; (merge-with into {} {1 [90]} {2 [-2]} {1 [100]}) ;=> {1 [90 100], 2 [-2]}
; ```
; That is, it takes a bunch of maps with a single key and a vector value `{k
; [v]}` and produces a single map whose
; - *keys* are the keys from the original maps, and whose
; - *values* are vectors made by `conj`ing all values with the same key.
; The desired behavior is obtained because `(into [90] [100])` is `[90 100]`.
;
; With that as prelude, the following uses the trick above. Instead of the maps
; provided to the function, we produce a vector of maps and then use the trick
; where `(f m1 m2 m3 ...)` is equivalent to `(apply f [m1 m2 m3 ...])`.
;
; We build the vector of maps using `mapcat` instead of `map` because each
; JMDICT entry that we're mapping over contains multiple readings. That is,
; we're mapping over a vector of entries each of which contains a vector of
; readings: `[[e1-r1 e1-r2 e1-r3] [e2-r1] [e3-r1 e3-r2] ...]`.
;
; Each map inside the vector output by `mapcat` has key as a reading and its
; corresponding value is a 1-tuple vector containing the entry number it
; corresponds to. Therefore, the final map is such that each key is a reading
; and its value is a vector of entry indexes.
(defn merge-with-vector-values [vector-of-maps] (apply merge-with into {} vector-of-maps))
(def reading-to-headword-idx
  (merge-with-vector-values
    (mapcat
      (fn [entry n]
        (mapv #(hash-map (:reb %) [n]) (:r_ele entry)))
      all-entries
      (range (count all-entries)))))

(def kanji-to-headword-idx
  (merge-with-vector-values
    (mapcat
      (fn [entry n]
        (if (:k_ele entry)
          (mapv #(hash-map (:keb %) [n]) (:k_ele entry))))
      all-entries
      (range (count all-entries)))))

; (-> "人" t/kanji-to-headwords)
(defn kanji-to-headwords
  [k]
  (->> k
       kanji-to-headword-idx
       (mapv all-entries)))

(defn reading-to-headwords
  [r]
  (->> r
       reading-to-headword-idx
       (mapv all-entries)))

(defn entry-common?
  [entry]
  (->> (concat (->> entry :k_ele (map :ke_pri))
               (->> entry :r_ele (map :re_pri)))
       (some identity)
       boolean))

(defn entry-has-kanji? [entry]
  (-> entry :k_ele some?))

(defn entry-to-kanjis [entry] (->> entry :k_ele (mapv :keb)))
(defn entry-to-readings [entry] (->> entry :r_ele (mapv :reb)))
(defn entry-to-kanji-cross-readings [entry]
  (for [k (entry-to-kanjis entry)
        r (entry-to-readings entry)]
    [k r]))

(defn entry-to-furigana [entry]
  (let [headword-reading-list (entry-to-kanji-cross-readings entry)]
    (map headword-reading-to-furigana headword-reading-list)))

(defn augment-entry-with-furigana [entry]
  (let [furiganas (entry-to-furigana entry)]
    (assoc entry :furigana furiganas)))

;;; Search!

(defn morpheme-to-headwords
  [morpheme]
  (map
    augment-entry-with-furigana
    (let [lemma-head (-> morpheme :lemma (string/split ,,, #"-") first)]
      (if (kana/any-kanji? lemma-head)
        ; some kanji
        (let [kanji-headwords (kanji-to-headwords lemma-head)
              reading-headwords (reading-to-headwords (:lemma_reading morpheme))
              headwords (distinct (concat kanji-headwords reading-headwords))]
          ; sort common to uncommon
          (sort-by (comp not entry-common?) headwords))
        ; no kanji
        (let [lemma-headwords (reading-to-headwords lemma-head)
              literal-headwords (reading-to-headwords (:literal morpheme))
              headwords (distinct (concat lemma-headwords literal-headwords))]
          ; sort common-to-uncommon (-2 if common) and no-kanji-to-kanji (-1 if
          ; lacking kanji). More negative comes first, since sort-by returns
          ; ascending.
          (sort-by #(+ (if (entry-has-kanji? %) 0 -1) (if (entry-common? %) -2 0)) headwords))))))

(defn text-to-headwords
  [text]
  (map
    augment-entry-with-furigana
    (let [kanji-headwords (kanji-to-headwords text)
          reading-headwords (reading-to-headwords text)
          headwords (distinct (concat kanji-headwords reading-headwords))]
      ; sort common to uncommon
      (sort-by (comp not entry-common?) headwords))))

(defn lexeme-to-headwords
  [{:keys [raw-text morphemes] :as lex}]
  (map
    augment-entry-with-furigana
    (let [headwords (mapcat morpheme-to-headwords morphemes)]
      (if (-> morphemes count (= 1))
        ; NOT multi-morpheme: all done
        headwords
        ; multi-morpheme: search for raw-text too:
        (concat (text-to-headwords raw-text) headwords)))))
;;;;;;;;;; TODO thread in jmdict-furigana into all hits here.

