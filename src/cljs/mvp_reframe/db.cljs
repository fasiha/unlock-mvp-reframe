(ns mvp-reframe.db
  (:require [cljs.reader]))

(def default-db
  {:new-japanese ""
   :new-translation ""
   :sentences (sorted-map)})

(def lsk "mvp-reframe")     ;; localstore key

(defn convert-db-maps-to-sorted-maps [db]
  (update-in db [:sentences] #(into (sorted-map) %)))

(defn ls->db
  "Read in db from LS, and process into a map we can use as app-db."
  []
  (some->> (.getItem js/localStorage lsk)
           (cljs.reader/read-string)          ;; stored as EDN maps
           (convert-db-maps-to-sorted-maps))) ;; maps -> sorted-maps

(defn db->ls!
  "Puts db into localStorage"
  [db]
  (.setItem js/localStorage lsk (str db)))   ;; caveat: sorted-maps writen as EDN maps
