(ns mvp-reframe.db
  (:require [cljs.reader]))

(def default-db
  {:new-japanese "" ; Japanese textbox
   :new-translation "" ; translation textbox
   :sentences (sorted-map) ; sorted-set: integer keys and values per default-sentence schema
   :sentence-id-surgery -1 ; integer key for sentence being edited (surgery)
   :lexeme-being-looked-up {} ; a lexeme in the sentence being edited to look up in JMdict
   :jmdict-entries [] ; server response: JMdict entries matching a given lexeme/morpheme
   })

(def default-sentence {:id -1           ; unique, currently int, FIXME make this UUID
                       :japanese ""     ; string
                       :translation ""  ; string
                       :raw-parse []    ; UniDic (or similar) morphemes
                       :tagged-parse [] ; vector of elements following default-tagged-parse schema
                       })

(def default-tagged-parse {:children []  ; vec containing other maps like this (recursive)
                           :tags []      ; vec of tags
                           :raw-text ""  ; raw string
                           :morphemes [] ; vec of UniDic morphemes, like :raw-parse above
                           :path []      ; vec of integers: descent tree from the sentence
                           })
; TODO add :furigana?

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
(defn flatten-taggables [taggables]
  (if (and (-> taggables empty? not) (every? identity taggables))
     (into taggables (flatten-taggables (mapcat :children taggables)))))

(defn pathed-taggable-to-pathed-children
  "Given a taggable with a :path, return its :children as a vector of taggables
  with their own :path key populated."
  [taggable]
  (map-indexed
    #(assoc %2 :path (conj (:path taggable) %1))
    (:children taggable)))

(defn add-paths-to-taggables
  "Given a vector of taggables, any of whose elements might have a :children key
  containing another vector of taggables, return a flattened vector of taggables
  including the descendants of the input. A user should use the 1-arity
  function. The 2-arity function is used internally."
  ([taggables]
   (add-paths-to-taggables ; call the 2-arity function after adding :path to input
     (mapv #(assoc %1 :path [%2])
           taggables
           (range (count taggables)))
     true))
  ([taggables with-path?]
   (if (and (-> taggables empty? not) (every? identity taggables))
     (mapv #(assoc % :children
                   (-> %
                       pathed-taggable-to-pathed-children
                       (add-paths-to-taggables ,,, true)))
           taggables)
     [])))

(defn fuse-wrap
  [operation taggables idx]
  (add-paths-to-taggables
    (if (and (nth taggables (+ 1 idx) false) ; merge idx and (1+idx)th entries
             (or (= operation :fuse) (= operation :wrap)))
      (let [[left right] (split-at idx taggables)
            [a b] (take 2 right)
            right (drop 2 right)
            middle (make-taggable (str (:raw-text a) (:raw-text b))
                                  (into (:morphemes a) (:morphemes b))
                                  []
                                  (if (= operation :fuse)
                                    []
                                    [a b]))]
        (apply conj (vec left) middle right))
      ; TODO just return the original input if either/both argument checks fail?
      taggables)))

(def fuse (partial fuse-wrap :fuse))
(def wrap (partial fuse-wrap :wrap))

(defn unfuse-unwrap
  [operation taggables idx]
  (add-paths-to-taggables
    (if (and (nth taggables idx false)
             (or (= operation :unfuse) (= operation :unwrap)))
      (let [[left right] (split-at idx taggables)
            wrapped-or-fuseed (first right)
            right (rest right)
            unwrapped-or-unfuseed (if (= operation :unwrap)
                                    (:children wrapped-or-fuseed)
                                    (init-tagged-parse {:words (:morphemes wrapped-or-fuseed)}))]
        (into [] (concat left unwrapped-or-unfuseed right)))
      taggables)))

(def unwrap-in-tagged-parse (partial unfuse-unwrap :unwrap))
(def unfuse-in-tagged-parse (partial unfuse-unwrap :unfuse))

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
