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
     [:button "Save"]]))

(defn main-panel []
  (let [] 
    (fn []
      [:div
       [:h3 "Sentence Editor"]
       [:p "Enter Japanese and an optional translation below. Separate sentences with two newlines."]
       [new-sentences-panel]
       [:h3 "Document Editor"]])))
