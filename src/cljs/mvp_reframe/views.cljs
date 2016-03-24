(ns mvp-reframe.views
  (:require [re-frame.core :as r]
            [clojure.string :as string]
            [garden.core :refer [css]]
            [mvp-reframe.kana-kanji :as kana]))

(defn new-sentences-panel []
  (let [new-japanese (r/subscribe [:new-japanese])
        new-translation (r/subscribe [:new-translation])]
    [:div#new-document-area
     [:h3 "Sentences adder"]
     [:textarea {:cols 40 :rows 5
                 :placeholder "日本語"
                 :value @new-japanese
                 :onChange #(r/dispatch [:new-text :new-japanese (-> % .-target .-value)])}]

     [:textarea {:cols 40 :rows 5
                 :placeholder "Translation"
                 :value @new-translation
                 :onChange #(r/dispatch [:new-text :new-translation (-> % .-target .-value)])}]

     [:button {:onClick #(r/dispatch [:submit-sentences @new-japanese @new-translation])}
      "Save"]]))

(def blacklisted-pos #{"SupplementarySymbol" "Whitespace"})

(defn fwp [s] (str "（" s "）")) ; i.e., full-width parens
(defn render-morpheme [{:keys [literal
                               literal_pronunciation
                               lemma
                               lemma_reading
                               pos
                               conjugation] :as morpheme}]
  (string/join
    (flatten
      [literal
       (when (kana/any-kanji? literal) (fwp (kana/katakana-to-hiragana literal_pronunciation) ))
       (when (not= literal lemma)
         [" — "
          lemma
          " "
          (when (kana/any-kanji? lemma) (fwp (kana/katakana-to-hiragana lemma_reading)))])
       "↔ "
       (string/join "/" pos)
       " - "
       (string/join "/" conjugation)])))

(defn morphemes-joinable? [a b]
  (let [apos (get-in a [:pos 0])
        bpos (get-in b [:pos 0])]
    (not (or (blacklisted-pos apos)
             (blacklisted-pos bpos)))))

(defn sentence-surgeon-panel []
  [:div.sentence-surgeon
   [:h3 "Sentence Surgeon"]
   (if-let [sentence (deref (r/subscribe [:sentence-for-surgery]))]
     [:div
      [:div.original-sentence (str (:japanese sentence))]
      [:div.sentence-split-merge
       (map-indexed
         (fn [idx [word next-word]]
           ^{:key (get-in word [:morphemes 0 :position])}
           [:span
            [:span.word (:raw-text word)]
            (if (> (count (:morphemes word)) 1)
              [:sub.unseparator {:onClick #(r/dispatch
                                             [:unmerge-tagged-parse
                                              (:id sentence)
                                              idx])} "×"])
            (if (morphemes-joinable? (-> word :morphemes last)
                                     (-> next-word :morphemes first))
              [:sub.separator {:onClick #(r/dispatch
                                           [:merge-tagged-parse
                                            (:id sentence)
                                            idx])} "+"])
            ]
           )
         (partition 2 1 [nil] (:tagged-parse sentence)))
       ]

      [:ul
       (for [lexeme (:tagged-parse sentence)
             :when (-> lexeme :morphemes first :pos first blacklisted-pos not)]
         ^{:key (get-in lexeme [:morphemes 0 :position])}
         [:li (:raw-text lexeme)
          [:button {:onClick #(r/dispatch [:lookup lexeme])} "lookup"]
          (into [] (concat
                     [:ul]
                     (map (fn [morpheme]
                            [:li (render-morpheme morpheme)])
                          (:morphemes lexeme))))])]

      ]
     [:div "…"])])

(defn sentences-list-panel []
  (let [sentences (r/subscribe [:sentences])] ; the sub calls `vals`
    [:div.sentences-list
     [:h3 "Sentences list"]
     [:div {:style {:border "1px solid lightgray" :height "10em" :overflowY "scroll"}}
      (for [sentence @sentences]               ; map would also work
        ^{:key (:id sentence)}
        [:div.sentence-japanese-translation
         [:button {:onClick #(r/dispatch [:delete-sentence (:id sentence)])} "×"]
         [:button {:onClick #(r/dispatch [:send-sentence-to-surgery (:id sentence)])} "✎"]
         (:japanese sentence) "—" (:translation sentence)
         ])
      ]]))

(defn sentence-editor-panel []
  [:div.sentence-editor
   [new-sentences-panel]
   [:div.sentence-list-surgeon
    [sentences-list-panel]
    [sentence-surgeon-panel]]
   ])

(defn make-css []
  (css [[:.original-sentence :.sentence-split-merge {:white-space "pre-wrap"}]
        [:.separator :.unseparator {:color "lightgray"}]]))

(defn main-panel []
  (let []
    (fn []
      [:div
       [:style (make-css)]
       [:h2 "Sentence Editor"]
       [:p "Enter Japanese and an optional translation below. Separate sentences with two newlines."]
       [sentence-editor-panel]
       [:h2 "Document Editor"]])))
