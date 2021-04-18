(ns simple.reframe-core
  (:require [reagent.dom :as dom]
            [re-frame.core :as rf]
            [re-frame.db :as rf-db]
            [clojure.string :as str]))

;; -- Domino 2 - Event Handlers -----------------------------------------------

(rf/reg-cofx
  :now
  (fn [coeffects]
    (assoc coeffects :now (js/Date.))))

(rf/reg-fx
  :db-changes
  (fn [changes]
    (when (seq changes)
      (swap! rf-db/app-db (fn [db]
                            (reduce (fn [db [path value]]
                                      (assoc-in db path value))
                                    db
                                    changes))))))

(rf/reg-event-fx
 :initialize
 [(rf/inject-cofx :now)]
 (fn [{:keys [now]} _]
   {:db-changes {[:time] now
                 [:time-color] "#f88"}}))

(rf/reg-event-fx
 :time-color-change
 (fn [{:keys [db]} [_ new-color-value]]
   {:db-changes {[:time-color] new-color-value}}))

(rf/reg-event-fx
  :update-timer
  [(rf/inject-cofx :now)]
  (fn [{:keys [db now]} _]
    {:db-changes {[:time] now}}))


;; -- Domino 4 - Query  -------------------------------------------------------

(rf/reg-sub
 :time
 (fn [db _]
   (:time db)))

(rf/reg-sub
 :time-color
 (fn [db _]
   (:time-color db)))


;; -- Domino 5 - View Functions ----------------------------------------------

(defn clock []
  [:div.example-clock
   {:style {:color @(rf/subscribe [:time-color])}}
   (-> @(rf/subscribe [:time])
       .toTimeString
       (str/split " ")
       first)])

(defn color-input []
  [:div.color-input
   "Time color: "
   [:input {:type "text"
            :value @(rf/subscribe [:time-color])
            :on-change #(rf/dispatch [:time-color-change (-> % .-target .-value)])}]])  ;; <---

(defn ui []
  [:div
   [:h1 "Hello world, it is now"]
   [clock]
   [color-input]])

;; -- Entry Point -------------------------------------------------------------

(defn render []
  (dom/render [ui] (js/document.getElementById "app")))

(defn ^:dev/after-load after-load []
  (rf/clear-subscription-cache!)
  (render))

(defn run []
  (rf/dispatch-sync [:initialize])
  (js/setInterval #(rf/dispatch [:update-timer]) 1000)
  (render))
