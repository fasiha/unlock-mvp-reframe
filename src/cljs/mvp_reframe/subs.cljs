(ns mvp-reframe.subs
    (:require-macros [reagent.ratom :refer [reaction]])
    (:require [re-frame.core :as re-frame]))

(re-frame/register-sub :db (fn [db] (reaction @db)))
(re-frame/register-sub :new-japanese (fn [db] (reaction (:new-japanese @db))))
(re-frame/register-sub :new-translation (fn [db] (reaction (:new-translation @db))))
(re-frame/register-sub :sentences (fn [db] (reaction (vals (:sentences @db)))))
(re-frame/register-sub :sentence-for-surgery (fn [db] (reaction (get-in @db [:sentences (:sentence-id-surgery @db)]))))
(re-frame/register-sub :jmdict-entries (fn [db] (reaction (:jmdict-entries @db))))
(re-frame/register-sub :lexeme-being-looked-up (fn [db] (reaction (:lexeme-being-looked-up @db))))
