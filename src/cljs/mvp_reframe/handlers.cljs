(ns mvp-reframe.handlers
    (:require [re-frame.core  :as re-frame :refer [after debug]]
              [clojure.string :as string]
              [mvp-reframe.db :as db       :refer [ls->db db->ls! default-db]]
              [mvp-reframe.kana-kanji :as kana]
              [goog.net.XhrIo :as xhr]
              [cognitect.transit :as transit]
              [clojure.walk :as walk]))

(def json-reader (transit/reader :json))
(def transit-writer (transit/writer :json))
(def my-transit-reader (partial transit/read json-reader))
(def my-transit-writer (partial transit/write transit-writer))

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
    (let [raw-parse (walk/keywordize-keys (my-transit-reader response))
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
  :fuse-tagged-parse
  middlewares
  (fn [db [_ idx]]
    (update-in db [:sentences (:sentence-id-surgery db) :tagged-parse] db/fuse ,,, idx)))

(re-frame/register-handler
  :wrap-tagged-parse
  middlewares
  (fn [db [_ idx]]
    (update-in db [:sentences (:sentence-id-surgery db) :tagged-parse] db/wrap ,,, idx)))

(re-frame/register-handler
  :unwrap-tagged-parse
  middlewares
  (fn [db [_ idx]]
    (update-in db [:sentences (:sentence-id-surgery db) :tagged-parse] db/unwrap-in-tagged-parse ,,, idx)))

(re-frame/register-handler
  :unfuse-tagged-parse
  middlewares
  (fn [db [_ idx]]
    (update-in db [:sentences (:sentence-id-surgery db) :tagged-parse] db/unfuse-in-tagged-parse ,,, idx)))

(re-frame/register-handler
  :ask-for-jmdict-lookup
  middlewares
  (fn [db [_ taggable]]
    (xhr/send "/jmdict"                                      ; URL
              #(re-frame/dispatch                            ; callback
                 [:lookup-response (-> % .-target .getResponseText my-transit-reader)])
              "POST"                                         ; HTTP method
              (my-transit-writer taggable)                   ; POST payload
              #js {"Content-Type" "application/transit+json" ; HTTP request headers
                   "Accept" "application/transit+json, */*"})
    (assoc db
           :taggable-being-tagged taggable
           :tags-results (:tags-results db/default-db))))

(re-frame/register-handler
  :ask-for-grammar-lookup
  middlewares
  (fn [db [_ taggable]]
    (assoc db
           :taggable-being-tagged taggable
           :tags-results {:source :grammar
                          :entries []})))

(re-frame/register-handler
  :new-grammar-entry
  middlewares
  (fn [db [_ text]]
    (->
      db
      (assoc-in ,,, [:new-grammar-entry] "")
      (update-in ,,, [:grammar-entries] conj ,,, {:id (keyword text) :name text}))))

(re-frame/register-handler
  :lookup-response
  middlewares
  (fn [db [_ entries]]
    (assoc db :tags-results {:source :jmdict :entries entries})))

(defn tag-taggable [db tag]
  (let [path (drop-last (interleave (:path (:taggable-being-tagged db)) (repeat :children)))
        full-path (concat [:sentences (:sentence-id-surgery db) :tagged-parse] path [:tags])]
    (update-in
      db
      full-path
      conj ,,, tag)))

(re-frame/register-handler
  :tag-taggable-with-grammar
  middlewares
  (fn [db [_ grammar-entry]]
    (tag-taggable db (db/make-grammar-tag grammar-entry))))

(re-frame/register-handler
  :tag-taggable-with-jmdict
  middlewares
  (fn [db [_ entry sense-number]]
    (tag-taggable db (db/make-jmdict-tag entry sense-number))))

(re-frame/register-handler
  :delete-tag
  middlewares
  (fn [db [_ taggable tag-idx]]
    (let [path (drop-last (interleave (:path taggable) (repeat :children)))
          full-path (concat [:sentences (:sentence-id-surgery db) :tagged-parse] path [:tags])]
      (update-in
        db
        full-path
        kana/drop-nth-vec ,,, tag-idx))))

