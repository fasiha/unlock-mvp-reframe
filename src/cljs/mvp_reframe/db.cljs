(ns mvp-reframe.db
  (:require [cljs.reader]))

(def default-db
  {:sentence-id-surgery -1
   :new-japanese ""
   :new-translation ""
   :sentences (sorted-map)})

(def default-sentence {:id -1          ; unique, currently int, FIXME make this UUID
                       :japanese ""    ; string
                       :translation "" ; string
                       :raw-parse []   ; UniDic (or similar) morphemes
                       :tagged-parse []
                       })

(def default-tagged-parse {:components [] ; list containing other maps like this (recursive)
                           :tags []       ; list of tags
                           :raw-text ""   ; raw string
                           :morphemes []  ; list of UniDic morphemes, like :raw-parse above
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

(defn -vec-str-aware-merge [a b] (if (coll? a) (into a b) (str a b)))
(defn merge-two-in-tagged-parse
  "In a vector of :tagged-parse maps, merge the two beginning at index idx"
  [tagged-parse idx]
  (if (<= idx (- (count tagged-parse) 2))
    (let [[left right] (split-at idx tagged-parse)
          [a b] (take 2 right)
          middle (merge-with -vec-str-aware-merge a b)
          right (drop 2 right)
          ]
      (apply conj (vec left) middle right))
    tagged-parse))

(defn unmerge-in-tagged-parse
  [tagged-parse idx]
  (let [[left right] (split-at idx tagged-parse)
        orig (nth tagged-parse idx)
        fin (init-tagged-parse {:words (:morphemes orig)})]
    (into [] (concat left fin (rest right)))))

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
