(ns mvp-reframe.views
  (:require [re-frame.core :as r]
            [clojure.string :as string]))

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

(defn sentence-surgeon-panel []
  [:div.sentence-surgeon {:style {:flexGrow 1}}
   [:h3 "Sentence Surgeon"]
   (if-let [sentence (deref (r/subscribe [:sentence-for-surgery]))]
     [:div (str (:japanese sentence))]
     [:div "…"])])

(defn sentences-list-panel []
  (let [sentences (r/subscribe [:sentences])] ; the sub calls `vals`
    [:div.sentences-list {:style {:flexGrow 1}}
     [:h3 "Sentences list"]
     [:div {:style {:border "1px solid lightgray" :height "10em" :overflow "scroll"}}
      (for [sentence @sentences]               ; map would also work
        ^{:key (:id sentence)}
        [:div
         [:button {:onClick #(r/dispatch [:delete-sentence (:id sentence)])} "×"]
         [:button {:onClick #(r/dispatch [:send-sentence-to-surgery (:id sentence)])} "✎"]
         (:japanese sentence) "—" (:translation sentence)
         ])
      ]]))

(defn sentence-editor-panel []
  [:div.sentence-editor
   [new-sentences-panel]
   [:div.sentence-list-surgeon {:style {:display "flex"}}
    [sentences-list-panel]
    [sentence-surgeon-panel]]
   ])

(defn main-panel []
  (let []
    (fn []
      [:div
       [:h2 "Sentence Editor"]
       [:p "Enter Japanese and an optional translation below. Separate sentences with two newlines."]
       [sentence-editor-panel]
       [:h2 "Document Editor"]])))
