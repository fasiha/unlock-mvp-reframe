(ns mvp-reframe.handlers
    (:require [re-frame.core :as re-frame :refer [after]]
              [mvp-reframe.db :as db :refer [ls->db db->ls! default-db]]))

(def ->ls (after db->ls!)) ;; middleware to store db into local storage

(re-frame/register-handler
 :initialize-db
 (fn  [_ _]
   (merge default-db (ls->db))))

(re-frame/register-handler
  :new-text
  ->ls
  (fn [db [_ path text]]
    (println "In :new-text handler")
    (merge db {path text})))

