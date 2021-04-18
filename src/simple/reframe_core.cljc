(ns simple.reframe-core
  (:require [reagent.core :as r]
            [reagent.dom :as dom]
            [re-frame.core :as rf]
            [re-frame.db :as rf-db]
            [clojure.string :as str]))

;; -- Setup - coeffects -------------------------------------------------------

(rf/reg-cofx
  :time/now
  (fn [coeffects]
    (assoc coeffects :time/now (js/Date.))))


;; -- Setup - effects ---------------------------------------------------------

(rf/reg-fx
  :vrac.db/changes
  (fn [changes]
    (when (seq changes)
      (swap! rf-db/app-db (fn [db]
                            (reduce (fn [db [path value]]
                                      (assoc-in db path value))
                                    db
                                    changes))))))


;; -- Setup - interceptors ----------------------------------------------------

(defn inject-new-ids [nb-ids]
  (rf/->interceptor
    :id :id-provider/new-ids
    :before (fn [context]
              (let [next-id (-> context :coeffects :db (:id-provider/next-id 0))
                    new-next-id (+ next-id nb-ids)]
                (assoc-in context [:coeffects :id-provider/new-ids] (vec (range next-id new-next-id)))))
    :after (fn [context]
             (let [coeffect-db (get-in context [:coeffects :db])
                   new-next-id (-> coeffect-db (:id-provider/next-id 0) (+ nb-ids))
                   effect-db (-> (get-in context [:effects :db] coeffect-db)
                                 (assoc :id-provider/next-id new-next-id))]
               (assoc-in context [:effects :db] effect-db)))))


;; -- Domino 2 - Event Handlers -----------------------------------------------

(rf/reg-event-db
 :initialize
 (fn [_ _]
   {:timer/by-id {}}))

(defn time-in-ms->display-value [ms]
  (-> (js/Date. ms)
      .toISOString
      (subs 11 19)))

(rf/reg-event-fx
  :timer/create
  [(inject-new-ids 1) (rf/inject-cofx :time/now)]
  (fn [{:keys [id-provider/new-ids time/now]} _]
    (let [[timer-id] new-ids
          timer-collection-path [:timer/by-id]]
      {:vrac.db/changes {(conj timer-collection-path timer-id) {:timer/id timer-id
                                                                :timer/start-time now
                                                                :timer/display-value (time-in-ms->display-value 0)
                                                                :color "#888"}}})))

(rf/reg-event-fx
  :timer/delete
  (fn [{:keys [db]} [_ timer-id]]
    (let [timer-collection-path [:timer/by-id]
          timers (get-in db timer-collection-path)]
      {:vrac.db/changes {timer-collection-path (dissoc timers timer-id)}})))

(rf/reg-event-fx
 :timer/change-color
 (fn [{:keys [db]} [_ color-path new-color]]
   {:vrac.db/changes {color-path new-color}}))

(rf/reg-event-fx
 :timer/update-current-time
 [(rf/inject-cofx :time/now)]
 (fn [{:keys [db time/now]} [_ timer-path]]
   (let [start-time-path (conj timer-path :timer/start-time)
         start-time (get-in db start-time-path)
         display-value (time-in-ms->display-value (- now start-time))]
     {:vrac.db/changes {(conj timer-path :timer/display-value) display-value}})))


;; -- Domino 4 - Query  -------------------------------------------------------

(rf/reg-sub
 :debug/db
 (fn [db _]
   db))

(rf/reg-sub
 :timer/all-ids
 (fn [db _]
   (-> db :timer/by-id keys)))

(rf/reg-sub
 :timer/by-id
 (fn [db [_ timer-id]]
   (get-in db [:timer/by-id timer-id])))


;; -- Domino 5 - View Functions ----------------------------------------------

(defn timer-comp [timer-id]
  (let [interval-handle (atom nil)]
    (r/create-class
      {:component-did-mount
       (fn [this]
         (->> (js/setInterval #(rf/dispatch [:timer/update-current-time [:timer/by-id timer-id]]) 1000)
              (reset! interval-handle)))

       :component-will-unmount
       (fn [this]
         (js/clearInterval @interval-handle))

       :reagent-render
       (fn [timer-id]
         (let [timer @(rf/subscribe [:timer/by-id timer-id])]
           [:<>
            [:div.example-clock
             {:style {:color (:color timer)}}
             (:timer/display-value timer)]
            [:div.color-input
             [:input {:type "text"
                      :value (:color timer)
                      :on-change #(rf/dispatch [:timer/change-color [:timer/by-id timer-id :color] (-> % .-target .-value)])}]]
            [:button {:on-click #(rf/dispatch [:timer/delete timer-id])} "Delete timer"]]))})))


(defn timer-list-comp []
  (let [timer-ids @(rf/subscribe [:timer/all-ids])]
    [:ul
     (for [timer-id timer-ids]
       ^{:key timer-id} [:li [timer-comp timer-id]])]))

(defn debug-comp []
  [:div (str @(rf/subscribe [:debug/db]))])

(defn ui []
  [:div
   [:h1 "Hello timers"]
   [:button {:on-click #(rf/dispatch [:timer/create])} "Create timer"]
   [timer-list-comp]
   [debug-comp]])

;; -- Entry Point -------------------------------------------------------------

(defn render []
  (dom/render [ui] (js/document.getElementById "app")))

(defn ^:dev/after-load after-load []
  (rf/clear-subscription-cache!)
  (render))

(defn run []
  (rf/dispatch-sync [:initialize])
  (rf/dispatch-sync [:timer/create])
  (render))
