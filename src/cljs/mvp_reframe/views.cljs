(ns mvp-reframe.views
  (:require [re-frame.core :as r]
            [clojure.string :as string]
            [garden.core :refer [css]]
            [mvp-reframe.kana-kanji :as kana]
            [mvp-reframe.db :as db]
            [mvp-reframe.config :as config :refer [debug?]]))

(defn make-ruby
  ([base furigana]
   (make-ruby base furigana "[" "]"))
  ([base furigana left right]
   [:ruby base
    [:rp left]
    [:rt furigana]
    [:rp right]]))

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

(defn entry-to-headwords
  [{:keys [k_ele r_ele] :as entry}]
  (let [kanjis (map :keb k_ele)
        readings (map :reb r_ele)
        titles (concat kanjis readings)]
    (string/join "・" titles)))

(defn jmdict-furigana-to-ruby [{:keys [headword reading furigana]}]
  (map
    (fn [{:keys [kanji reading]}] ; inner reading
      (with-meta
        (if kanji
          (make-ruby kanji reading)
          [:span reading]) {:key (str kanji reading)}))
    furigana))

(defn render-jmdict-entry
  [{:keys [k_ele r_ele sense ent_seq] :as entry}]
  (let [title (entry-to-headwords entry)]
    [:div title

     [:div
      (map-indexed (fn [n f]
                     ^{:key (str ent_seq n)}
                     [:span {:style {:marginRight "1em"}}
                      (jmdict-furigana-to-ruby f)
                      [:button {:onClick #(r/dispatch [:add-furigana-to-taggable (:furigana f)])} "+"]])
                   (:furigana entry))]

     [:ul
      (map-indexed
        (fn [sense-num a-sense]
          ^{:key (str ent_seq "," sense-num)}
          [:li (->> a-sense :gloss (string/join "； "))
           [:button.tag-button
            {:onClick #(r/dispatch [:tag-taggable-with-jmdict entry sense-num])}
            "tag"]])
        sense)]
     ]))

(defn entry-to-sense-via-num
  [entry sense-num]
  (-> entry
      :sense
      (nth sense-num)
      :gloss
      (->> (string/join "； "))))

(defn render-jmdict-tag
  [{:keys [entry sense-number]}]
  [:span.tag.jmdict
   (entry-to-headwords entry)
   " ⇒ "
   (entry-to-sense-via-num entry sense-number)])

(defn render-tag
  [{:keys [tag source]}]
  (condp = source
    :jmdict (render-jmdict-tag tag)
    :grammar [:span.tag.grammar (-> tag :entry :name)]
    , [:span (pr-str tag)]))

(defn morphemes-joinable? [a b]
  (let [apos (get-in a [:pos 0])
        bpos (get-in b [:pos 0])]
    (not (or (blacklisted-pos apos)
             (blacklisted-pos bpos)))))

(defn render-taggables-as-sentence
  "An outer function should call this with level=0 and an arbitrarily-long
  vector of taggables (& third argument idx-in-parent as nil). If/when the
  function recurses, level will be >0, taggables will have >=2 elements, and
  idx-in-parent will be set. Recursion happens whenever a taggable in taggables
  has children: idx-in-parent will then be the index of the taggable with
  children in its own list of siblings."
  [level taggables & [idx-in-parent]]
  #_(if debug? (println "render-taggables-as-sentence"
                        (map :raw-text taggables) "level" level "idx-in-parent" idx-in-parent))
  (let [num-taggables (count taggables)
        render-one
        (fn [level taggable idx]
          #_(if debug? (println "render-one"
                              (str level ","
                                   (-> taggable :morphemes first :position))
                              (-> taggable :raw-text)
                              "idx" idx))
          ^{:key (str level "," (-> taggable :morphemes first :position))}
          [:span.taggable (if (-> taggable :morphemes first :pos first blacklisted-pos not)
                            {:className "raw-text"})
           (if (-> taggable :children empty?)

             ; CASE 1: no children: render the taggable
             [:span.taggable
              (:raw-text taggable)
              (if (and (= level 0)
                       (-> taggable :morphemes count (not= 1)))
                [:button {:onClick #(r/dispatch [:unfuse-tagged-parse idx])} "unfuse"])]

             ; CASE 2: pass non-empty (thus >=2-long) vector of child taggables
             ; to render-taggables-as-sentence
             (render-taggables-as-sentence (+ 1 level) (:children taggable) idx))
           ]
          )]
    ^{:key level}
    [:span.taggables
     (mapcat
       (fn [taggable next-taggable idx]
         [; First, render the taggable:
          (render-one level taggable idx)

          ; Next. IF (1) we're at the top-level,
          ; AND
          ; if (2) this isn't the last taggable,
          ; AND
          ; if (3) this taggable & next-taggable's adjacent morphemes aren't
          ; blacklisted (whitespace, etc.),
          ; THEN make buttons to allow the two taggables to be:
          ;
          ; - fuseed: replace with new taggable with NO children, and with no
          ; tags and concatenated morphemes, or
          ; - wrapped: make a new taggable WITH the two taggables as children,
          ; and again without tags and with concatenated morphemes.
          ;
          ; SO, if you made a mistake and need to wrap/fuse/unfuse taggables
          ; which are already children of other taggables, then tough luck: you
          ; have to unwrap/unfuse all ancestor taggables till this taggable &
          ; its neighbor are at the top-level, and deal with them from the
          ; beginning. FIXME maybe?
          (if (and (= level 0)
                   (not= idx (- num-taggables 1))
                   (morphemes-joinable? (-> taggable :morphemes last)
                                        (-> next-taggable :morphemes first)))
            ^{:key (str "buttons" level ","  (-> taggable :morphemes first :position))}
            [:span
             [:button {:onClick #(r/dispatch [:fuse-tagged-parse idx])} "fuse"]
             [:button {:onClick #(r/dispatch [:wrap-tagged-parse idx])} "wrap"]])])
       taggables
       (-> taggables (subvec ,,, 1) (conj ,,, nil))
       (range num-taggables))
     ; A vector of taggables should be unwrappable, unless it's the very
     ; top-level
     (if (= level 1) [:button
                      {:onClick #(r/dispatch [:unwrap-tagged-parse idx-in-parent])}
                      "unwrap"])]))

(defn taggable-to-ruby [{:keys [raw-text furigana] :as taggable}]
  (if furigana
    (map-indexed (fn [n {:keys [kanji reading]}]
                   (with-meta
                     (if kanji
                       (make-ruby kanji reading)
                       [:span reading])
                     {:key (str "rubies-" n)}))
                 furigana)
    raw-text))

(defn render-taggables-as-list [sentence]
  [:ul
   (map-indexed
     (fn [idx taggable]
       ^{:key (str idx (:raw-text taggable))}
       [:li.tags-and-morphemes
        (taggable-to-ruby taggable)

        ; Buttons
        [:button {:onClick #(r/dispatch [:ask-for-jmdict-lookup taggable])} "jmdict"]
        [:button {:onClick #(r/dispatch [:ask-for-grammar-lookup taggable])} "grammar"]

        ; Tags
        (if (-> taggable :tags empty? not)
          [:div.tag-list "Tags"
           [:ul
            (map-indexed
              (fn [tag-idx tag]
                ^{:key (str idx "tag" tag-idx)}
                [:li (render-tag tag)
                 [:button
                  {:onClick #(r/dispatch
                               [:delete-tag taggable tag-idx])}
                  "×"]])
              (:tags taggable))
            ]])


        ; morphemes
        [:div.morpheme-list "Morphemes "
         [:ol
          (map-indexed
            (fn [m-idx morpheme]
              ^{:key (str idx "m" m-idx)}
              [:li.morpheme-in-taggable (render-morpheme morpheme)])
            (:morphemes taggable))]]
        ])
     (->> sentence
          :tagged-parse
          db/flatten-taggables
          (filter #(-> %
                       :morphemes
                       first
                       :pos
                       first
                       blacklisted-pos
                       not))))])

(defn render-grammar-entry [entry]
  [:div (:name entry)
   [:button {:onClick #(r/dispatch [:tag-taggable-with-grammar entry])} "tag"]
   ])

(defn sentence-surgeon-panel []
  [:div.sentence-surgeon
   [:h3 "Sentence Surgeon"]
   (if-let [sentence (deref (r/subscribe [:sentence-for-surgery]))]
     [:div
      ; Display the original sentence
      [:div.original-sentence (str (:japanese sentence))]

      ; Render it with nested taggables, allowing user to fuse/wrap or
      ; unfuse/unwrap
      [:div (render-taggables-as-sentence 0 (:tagged-parse sentence))]

      [:div.flexible-container
       ; A list of taggables, with tags and the ability to add more tags
       [:div.flexible-containee {:style {:border "1px solid lightgray" :maxHeight "30em" :overflowY "scroll" :flex 1}}
        (render-taggables-as-list sentence)
        ]

       ; The results of tag lookups: JMdict, grammar, etc.
       (let [{:keys [entries source] :as results} @(r/subscribe [:tags-results])
             taggable-under-lookup @(r/subscribe [:taggable-being-tagged])]
         [:div.flexible-containee {:style {:border "1px solid lightgray" :maxHeight "30em" :overflowY "scroll" :flex 1}}
          [:h4 "Tag lookup " [:code (str source)]]
          [:div "Tagging: " (:raw-text taggable-under-lookup)]
          (condp = source
            :grammar (let [entries @(r/subscribe [:grammar-entries])
                           new-grammar-entry @(r/subscribe [:new-grammar-entry])]
                       [:div
                        [:ul (map
                               (fn [entry]
                                 ^{:key (:id entry)}
                                 [:li (render-grammar-entry entry)])
                               entries)]
                        [:input {:type "text"
                                 :placeholder "New/search? grammar entry"
                                 :value new-grammar-entry
                                 :onChange #(r/dispatch [:new-text
                                                         :new-grammar-entry
                                                         (-> % .-target .-value)])}]
                        [:button {:onClick #(r/dispatch [:new-grammar-entry
                                                         new-grammar-entry])}
                         "+"]])

            :jmdict [:ul (map
                           (fn [entry]
                             ^{:key (:ent_seq entry)}
                             [:li (render-jmdict-entry entry)])
                           entries)]
            nil
            )])
       ]

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
  (css [
        [:div.flexible-container {:display "flex" :flex-direction "row"}]
        [:div.flexible-containee {:flex-grow 1}]
        [:div.sentence-japanese-translation {:white-space "pre-line"}]
        [:div.tag-list :div.morpheme-list
         {:margin "0.25em 0.25em 0.25em 1em"
          :padding "0.1em"
          :border "1px lightgray dotted"}]
        [:li.tags-and-morphemes {:padding-bottom "1em"}]
        [:div.tag-list :div.morpheme-list
         {:font-size "small"}]
        [:.raw-text::before {:content "\"[\""}]
        [:.raw-text::after {:content "\"]\""}]
        [:.original-sentence :.sentence-split-merge {:white-space "pre-wrap"}]
        [:.separator :.unseparator {:color "lightgray"}]]))

(defn main-panel []
  (let []
    (fn []
      [:div
       [:style {:dangerouslySetInnerHTML {:__html (make-css) }}]
       [:h2 "Sentence Editor"]
       [:p "Enter Japanese and an optional translation below. Separate sentences with two newlines."]
       [sentence-editor-panel]
       [:h2 "Document Editor"]])))
