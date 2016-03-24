(ns mvp-reframe.db
  (:require [cljs.reader]))

(def default-db
  {:new-japanese "" ; Japanese textbox
   :new-translation "" ; translation textbox
   :sentences (sorted-map) ; sorted-set: integer keys and values per default-sentence schema
   :sentence-id-surgery -1 ; integer key for sentence being edited (surgery)
   :lexeme-being-looked-up {} ; a lexeme in the sentence being edited to look up in JMdict
   :jmdict-headwords [] ; server response: JMdict headwords matching a given lexeme/morpheme
   })

(def default-sentence {:id -1           ; unique, currently int, FIXME make this UUID
                       :japanese ""     ; string
                       :translation ""  ; string
                       :raw-parse []    ; UniDic (or similar) morphemes
                       :tagged-parse [] ; vector of elements following default-tagged-parse schema
                       })

(def default-tagged-parse {:children []  ; list containing other maps like this (recursive)
                           :tags []      ; list of tags
                           :raw-text ""  ; raw string
                           :morphemes [] ; list of UniDic morphemes, like :raw-parse above
                           })

;; Helper functions

(defn convert-db-maps-to-sorted-maps
  "Convert the [:sentences] path from a EDN map to a sorted-map"
  [db]
  (update-in db [:sentences] #(into (sorted-map) %)))

(defn enforce-sentence-schema
  "Ensure all sentences have a base schema, per default-sentence"
  [db]
  (assoc-in db [:sentences]
            (into
              (:sentences db)
              (map
                (fn [[k v]] [k (merge default-sentence v)])
                (:sentences db)))))

(defn init-tagged-parse
  "Convert `(:raw-parse sentence)`, a map of `{:words UniDic-vec}` where
  `UniDic-vec` is a vector of raw UniDic parse maps, to `(:tagged-parse
  sentence)`"
  [raw-parse]
  (mapv #(merge default-tagged-parse
                {:raw-text (:literal %) :morphemes [%]})
        (:words raw-parse)))

(defn make-taggable [raw-text morphemes tags children] {:raw-text raw-text 
                                                        :morphemes morphemes 
                                                        :tags tags 
                                                        :children children})
(defn squash-wrap [operation taggables idx]
  (if (and (nth taggables (+ 1 idx) false) ; merge idx and (1+idx)th entries
           (or (= operation :squash) (= operation :wrap)))
    (let [[left right] (split-at idx taggables)
          [a b] (take 2 right)
          right (drop 2 right)
          middle (make-taggable (str (:raw-text a) (:raw-text b))
                                (into (:morphemes a) (:morphemes b))
                                []
                                (if (= operation :squash) 
                                  [] 
                                  [a b]))]
      (apply conj (vec left) middle right))
    ; TODO just return the original input if either/both argument checks fail?
    taggables))

(def squash (partial squash-wrap :squash))
(def wrap (partial squash-wrap :wrap))

(defn unwrap-in-tagged-parse
  [taggables idx]
  (let [[left right] (split-at idx taggables)
        wrapped (first right)
        right (rest right)
        unwrapped (:children wrapped)]
    (into [] (concat left unwrapped right))))

(defn unsquash-in-tagged-parse
  [taggables idx]
  (let [[left right] (split-at idx taggables)
        squashed (nth taggables idx)
        right (rest right)
        unsquashed (init-tagged-parse {:words (:morphemes squashed)})]
    (into [] (concat left unsquashed right))))

;; localStorage-wrangling middleware

(def lsk "mvp-reframe")     ;; localstore key

(defn ls->db
  "Read in db from LS, and process into a map we can use as app-db."
  []
  (some->> (.getItem js/localStorage lsk)
           (cljs.reader/read-string)          ;; stored as EDN maps
           (convert-db-maps-to-sorted-maps)   ;; maps -> sorted-maps
           (enforce-sentence-schema)
           ))

(defn db->ls!
  "Puts db into localStorage"
  [db]
  (.setItem js/localStorage lsk (str db)))   ;; caveat: sorted-maps writen as EDN maps
