(ns mvp-reframe.views
  (:require [re-frame.core :as r]
            [clojure.string :as string]))

(defn new-sentences-panel []
  (let [new-japanese (r/subscribe [:new-japanese])
        new-translation (r/subscribe [:new-translation])]
    [:div#new-document-area
     [:textarea {:placeholder "日本語"
                 :value @new-japanese
                 :onChange #(r/dispatch [:new-text :new-japanese (-> % .-target .-value)])}]
     [:textarea {:placeholder "Translation"
                 :value @new-translation
                 :onChange #(r/dispatch [:new-text :new-translation (-> % .-target .-value)])}]
     [:button {:onClick #(r/dispatch [:submit-sentences @new-japanese @new-translation])} "Save"]]))

(defn sentences-list-panel []
  (let [sentences (r/subscribe [:sentences])] ; the sub calls `vals`
    (println @sentences)
    [:div.sentences-list
     [:ul
      (for [s @sentences]               ; map would also work
        ^{:key (:id s)}
        [:li
         [:button {:onClick #(r/dispatch [:delete-sentence (:id s)])} "×"]
         (:japanese s) "—" (:translation s)
         ])
      ]]))

(defn sentence-editor-panel []
  [:div.sentence-editor
   [new-sentences-panel]
   [sentences-list-panel]
   ])

(defn main-panel []
  (let []
    (fn []
      [:div
       [:h3 "Sentence Editor"]
       [:p "Enter Japanese and an optional translation below. Separate sentences with two newlines."]
       [sentence-editor-panel]
       [:h3 "Document Editor"]])))
