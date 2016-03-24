(ns mvp-reframe.taggers
  (:require [clojure.java.io :as io]
            [clojure.walk :as walk]
            [clojure.string :as string]
            [cheshire.core :as json]
            [mvp-reframe.kana-kanji :as kana]))

(def filename "JMdict-full.ldjson")

(def all-entries
  (->>
    filename
    io/resource
    slurp
    string/trim
    string/split-lines
    (mapv json/parse-string)
    walk/keywordize-keys))

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

(defn morpheme-to-headwords
  [morpheme]
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
        (sort-by #(+ (if (entry-has-kanji? %) 0 -1) (if (entry-common? %) -2 0)) headwords)))))

(defn text-to-headwords
  [text]
  (let [kanji-headwords (kanji-to-headwords text)
        reading-headwords (reading-to-headwords text)
        headwords (distinct (concat kanji-headwords reading-headwords))]
    ; sort common to uncommon
    (sort-by (comp not entry-common?) headwords)))

(defn lexeme-to-headwords
  [lex]
  (let [raw-text (:raw-text lex)
        morphemes (:morphemes lex)]
    (if (-> morphemes count (= 1))
      ; NOT multi-morpheme
      (morpheme-to-headwords (first morphemes))
      ;
      (text-to-headwords raw-text))))
