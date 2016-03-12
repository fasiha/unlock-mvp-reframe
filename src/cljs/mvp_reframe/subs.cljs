(ns mvp-reframe.subs
    (:require-macros [reagent.ratom :refer [reaction]])
    (:require [re-frame.core :as re-frame]))

(re-frame/register-sub :new-japanese (fn [db] (reaction (:new-japanese @db))))
(re-frame/register-sub :new-translation (fn [db] (reaction (:new-translation @db))))
