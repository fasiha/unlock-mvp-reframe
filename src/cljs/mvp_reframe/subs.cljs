(ns mvp-reframe.subs
    (:require-macros [reagent.ratom :refer [reaction]])
    (:require [re-frame.core :as re-frame]))

(re-frame/register-sub :db (fn [db] (reaction @db)))
(re-frame/register-sub :new-japanese (fn [db] (reaction (:new-japanese @db))))
(re-frame/register-sub :new-translation (fn [db] (reaction (:new-translation @db))))
(re-frame/register-sub :sentences (fn [db] (reaction (vals (:sentences @db)))))
(re-frame/register-sub :sentence-for-surgery (fn [db] (reaction (get-in @db [:sentences (:sentence-id-surgery @db)]))))
(re-frame/register-sub :tags-results (fn [db] (reaction (:tags-results @db))))
(re-frame/register-sub :taggable-being-tagged (fn [db] (reaction (:taggable-being-tagged @db))))
(re-frame/register-sub :grammar-entries (fn [db] (reaction (:grammar-entries @db))))
(re-frame/register-sub :new-grammar-entry (fn [db] (reaction (:new-grammar-entry @db))))
