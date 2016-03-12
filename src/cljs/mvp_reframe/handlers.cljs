(ns mvp-reframe.handlers
    (:require [re-frame.core  :as re-frame :refer [after debug]]
              [mvp-reframe.db :as db       :refer [ls->db db->ls! default-db]]
              [clojure.string :as string]))

(def ->ls (after db->ls!)) ;; middleware to store db into local storage
(def middlewares [debug ->ls])  ;; add debug here, e.g. if needed

(re-frame/register-handler
 :initialize-db
 (fn  [_ _]
   (merge default-db (ls->db))))

(re-frame/register-handler
  :new-text
  middlewares
  (fn [db [_ path text]]
    (merge db {path text})))

(re-frame/register-handler
  :submit-sentences
  middlewares
  (fn [db [_ japanese translation]]
    (let [japanese-vec (string/split japanese #"\n\n")
          translation-vec (string/split translation #"\n\n")
          next-id ((fnil inc 0) (last (keys (:sentences db))))]
      (assoc-in
        db
        [:sentences]
        (into
          (:sentences db)
          (map (fn [j t n] (let [id (+ next-id n)]
                             {id {:id id :japanese j :translation t}}))
               japanese-vec
               translation-vec
               (range (count japanese-vec))))
        ))))

(re-frame/register-handler
  :delete-sentence
  middlewares
  (fn [db [_ id]]
    (update-in db [:sentences] dissoc id)))
