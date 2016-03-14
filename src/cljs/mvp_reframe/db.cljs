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
                       })

;; localStorage-wrangling middleware

(def lsk "mvp-reframe")     ;; localstore key

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
