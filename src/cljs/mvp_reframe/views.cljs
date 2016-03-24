(ns mvp-reframe.views
  (:require [re-frame.core :as r]
            [clojure.string :as string]
            [garden.core :refer [css]]
            [mvp-reframe.kana-kanji :as kana]
            [mvp-reframe.config :as config :refer [debug?]]))

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

(defn render-headword
  [{:keys [k_ele r_ele sense ent_seq] :as headword}]
  (let [kanjis (map :keb k_ele)
        readings (map :reb r_ele)
        titles (concat kanjis readings)
        title (string/join "・" titles)]
    [:div title
     [:ul
      (map-indexed
        (fn [n a-sense]
          ^{:key (str ent_seq "," n)}
          [:li (->> a-sense :gloss (string/join "； "))
           [:button.tag-button "tag"]])
        sense)]
     ]))

(defn morphemes-joinable? [a b]
  (let [apos (get-in a [:pos 0])
        bpos (get-in b [:pos 0])]
    (not (or (blacklisted-pos apos)
             (blacklisted-pos bpos)))))

(defn render-taggables
  "An outer function should call this with level=0 and an arbitrarily-long
  vector of taggables (& third argument idx-in-parent as nil). If/when the
  function recurses, level will be >0, taggables will have >=2 elements, and
  idx-in-parent will be set. Recursion happens whenever a taggable in taggables
  has children: idx-in-parent will then be the index of the taggable with
  children in its own list of siblings."
  [level taggables & [idx-in-parent]]
  (if debug? (println "render-taggables" (map :raw-text taggables) "level" level "idx-in-parent" idx-in-parent))
  (let [num-taggables (count taggables)
        render-one
        (fn [level taggable idx]
          (if debug? (println "render-one"
                   (str level ","
                        (-> taggable :morphemes first :position))
                   (-> taggable :raw-text)
                   "idx" idx))
          (if (-> taggable :children empty?)
            ; CASE 1: no children: render the taggable
            ^{:key (str level "," (-> taggable :morphemes first :position))}
            [:span.taggable
             "["
             (:raw-text taggable)
             "]"
             (if (and (= level 0)
                      (-> taggable :morphemes count (not= 1)))
               [:button {:onClick #(r/dispatch [:unfuse-tagged-parse idx])} "unfuse"])]

            ; CASE 2: pass non-empty (thus >=2-long) vector of child taggables
            ; to render-taggables
            (render-taggables (+ 1 level) (:children taggable) idx)))]
    ^{:key level}
    [:span.taggables
     "{"
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
     "}"
     ; A vector of taggables should be unwrappable, unless it's the very
     ; top-level
     (if (= level 1) [:button
                      {:onClick #(r/dispatch [:unwrap-tagged-parse idx-in-parent])}
                      "unwrap"])]))

(defn sentence-surgeon-panel []
  [:div.sentence-surgeon
   [:h3 "Sentence Surgeon"]
   (if-let [sentence (deref (r/subscribe [:sentence-for-surgery]))]
     [:div
      [:div.original-sentence (str (:japanese sentence))]

      [:div (render-taggables 0 (:tagged-parse sentence))]

      ; #_[:ul
      ;  (for [lexeme (:tagged-parse sentence)
      ;        :when (-> lexeme :morphemes first :pos first blacklisted-pos not)]
      ;    ^{:key (get-in lexeme [:morphemes 0 :position])}
      ;    [:li (:raw-text lexeme)
      ;     [:button {:onClick #(r/dispatch [:ask-for-lookup lexeme])} "lookup"]
      ;     (into [] (concat
      ;                [:ul]
      ;                (map (fn [morpheme]
      ;                       [:li (render-morpheme morpheme)])
      ;                     (:morphemes lexeme))))])]

      (let [headwords @(r/subscribe [:jmdict-headwords])
            lexeme-lookup @(r/subscribe [:lexeme-being-looked-up])]
        [:div
         [:h4 "JMDICT lookup"]
         [:p "Looking up: " (:raw-text lexeme-lookup)]
         [:ul (map
                (fn [headword]
                  ^{:key (:ent_seq headword)}
                  [:li (render-headword headword)])
                headwords)]])

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
        ;[:.taggable {:padding "0.5em"}]
        [:.taggables:hover {:background-color "rgb(240,240,240)"}]
        [:.taggables {:padding "0.5em"}]
        [:.taggable:hover {:background-color "rgb(225,225,225)"}]
        [:.original-sentence :.sentence-split-merge {:white-space "pre-wrap"}]
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
