(ns mvp-reframe.handlers
    (:require [re-frame.core  :as re-frame :refer [after debug]]
              [mvp-reframe.db :as db       :refer [ls->db db->ls! default-db]]
              [clojure.string :as string]
              [goog.net.XhrIo :as xhr]
              [cognitect.transit :as transit]
              [clojure.walk :as walk]))

(def json-reader (transit/reader :json))

(def ->ls (after db->ls!)) ;; middleware to store db into local storage
(def middlewares [->ls])  ;; add debug here, e.g. if needed

(re-frame/register-handler
 :initialize-db
 (fn  [_ _]
   (merge default-db (ls->db))))

(re-frame/register-handler
  :new-text
  middlewares
  (fn [db [_ path text]]
    (merge db {path text})))

(def UNIDIC_URL "/parse/")

(re-frame/register-handler
  :submit-sentences
  middlewares
  (fn [db [_ japanese translation]]
    (let [japanese-vec (string/split japanese #"\n\n")
          total (count japanese-vec)
          translation-vec (string/split translation #"\n\n")
          next-id ((fnil inc 0) (last (keys (:sentences db))))
          xhrs (map-indexed
                 (fn [n jap]
                   (xhr/send
                     (str UNIDIC_URL (js/encodeURIComponent jap))
                     #(re-frame/dispatch
                        [:parse-response
                         (+ next-id n)
                         (-> % .-target .getResponseText)])))
                 japanese-vec)
          ; for some reason, if we don't DO something with xhrs, they don't get
          ; run >.<, so save a count of them… WTF FIXME
          wtf (count xhrs)
          ]
      (assoc-in
        db [:sentences]
        (into
          (:sentences db)
          (map (fn [j t n]
                 (let [id (+ next-id n)]
                   {id {:id id :japanese j :translation t :raw-parse nil}}))
               japanese-vec
               (apply conj translation-vec (repeat total nil)) ; translations are optional
               (range (count japanese-vec))))))))

(re-frame/register-handler
  :parse-response
  middlewares
  (fn [db [_ id response]]
    (update-in
      db [:sentences id :raw-parse]
      merge (walk/keywordize-keys (transit/read json-reader response)))))

(re-frame/register-handler
  :delete-sentence
  middlewares
  (fn [db [_ id]]
    (update-in db [:sentences] dissoc id)))
