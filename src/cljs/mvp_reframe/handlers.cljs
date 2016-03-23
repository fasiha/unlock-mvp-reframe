(ns mvp-reframe.handlers
    (:require [re-frame.core  :as re-frame :refer [after debug]]
              [mvp-reframe.db :as db       :refer [ls->db db->ls! default-db]]
              [clojure.string :as string]
              [goog.net.XhrIo :as xhr]
              [cognitect.transit :as transit]
              [clojure.walk :as walk]))

(def json-reader (transit/reader :json))
(def transit-writer (transit/writer :json))

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
      ; Populate the sentence map with known fields now. The :raw-parse value
      ; will be populated by XHR response (see callback above).
      (assoc-in
        db [:sentences]
        (into
          (:sentences db)
          (map (fn [j t n]
                 (let [id (+ next-id n)]
                   {id (merge db/default-sentence {:id id :japanese j :translation t})}))
               japanese-vec
               (apply conj translation-vec (repeat total nil)) ; translations are optional
               (range (count japanese-vec))))))))

(re-frame/register-handler
  :parse-response
  middlewares
  (fn [db [_ id response]]
    (let [raw-parse (walk/keywordize-keys (transit/read json-reader response))
          tagged-parse (db/init-tagged-parse raw-parse)]
      (update-in
        db [:sentences id]
        merge {:raw-parse raw-parse :tagged-parse tagged-parse}))))

(re-frame/register-handler
  :send-sentence-to-surgery
  middlewares
  (fn [db [_ id]]
    (assoc-in db [:sentence-id-surgery] id)))

(re-frame/register-handler
  :delete-sentence
  middlewares
  (fn [db [_ id]]
    (update-in db [:sentences] dissoc id)))

(re-frame/register-handler
  :merge-tagged-parse
  middlewares
  (fn [db [_ id idx]]
    (update-in db [:sentences id :tagged-parse] db/merge-two-in-tagged-parse ,,, idx)))

(re-frame/register-handler
  :unmerge-tagged-parse
  middlewares
  (fn [db [_ id idx]]
    (update-in db [:sentences id :tagged-parse] db/unmerge-in-tagged-parse ,,, idx)))

(re-frame/register-handler
  :lookup
  middlewares
  (fn [db [_ lexeme]]
    (xhr/send "/jmdict"
              #(println "in CB!" (-> % .-target .getResponseText))
              "POST"
              (transit/write transit-writer lexeme)
              #js {"Content-Type" "application/transit+json"})
    ; no change for now.
    db))

