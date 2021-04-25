(ns simple.reframe-demo
  (:require [reagent.core :as r]
            [reagent.dom :as dom]
            [re-frame.core :as rf]
            [simple.util :refer [pp-str ensure-id]]))

;; -- Setup - coeffects -------------------------------------------------------

(rf/reg-cofx
  :time/now
  (fn [coeffects]
    (assoc coeffects :time/now (js/Date.))))


;; -- Domino 2 - Event Handlers -----------------------------------------------

(rf/reg-event-db
 :initialize-app-db
 (fn [_ _]
   ;; We use a normalized DB.
   {:entity/by-id {:timer-list {:id :timer-list
                                :timer-list/timers []}}}))

(defn time-in-ms->display-value [ms]
  (-> (js/Date. ms)
      .toISOString
      (subs 11 19)))

(rf/reg-event-fx
  :timer/create
  [(rf/inject-cofx :time/now)]
  (fn [{:keys [db time/now]} _]
    (let [timer (-> {:timer/start-time    now
                     :timer/display-value (time-in-ms->display-value 0)
                     :color               "#888"}
                    ensure-id)
          timer-id (:id timer)]
      ;; TODO: won't work when multiple timer-list can exist.
      ;; TODO: Introduce :timer/timer-lists and computed data.
      {:db (-> db
               (update :entity/by-id assoc timer-id timer)
               (update-in [:entity/by-id :timer-list :timer-list/timers] conj timer-id))})))

(rf/reg-event-db
  :timer/delete
  (fn [db [_ timer-id]]
    (-> db
        (update :entity/by-id dissoc timer-id)
        (update-in [:entity/by-id :timer-list :timer-list/timers]
                   (fn [timers]
                     (into [] (remove #{timer-id}) timers))))))

(rf/reg-event-db
  :timer/change-color
  (fn [db [_ timer-id new-color]]
    (assoc-in db [:entity/by-id timer-id :color] new-color)))

(rf/reg-event-fx
  :timer/update-current-time
  [(rf/inject-cofx :time/now)]
  (fn [{:keys [db time/now]} [_ timer-id]]
    {:db (update-in db [:entity/by-id timer-id]
                    (fn [timer]
                      (let [start-time (:timer/start-time timer)]
                        (assoc timer
                          :timer/display-value (time-in-ms->display-value (- now start-time))))))}))


;; -- Domino 4 - Query  -------------------------------------------------------

(rf/reg-sub
  :debug/db
  (fn [db _]
    db))

(rf/reg-sub
 :timers
 (fn [db _]
   (get-in db [:entity/by-id :timer-list :timer-list/timers])))

(rf/reg-sub
 :timer
 (fn [db [_ timer-id]]
   (get-in db [:entity/by-id timer-id])))


;; -- Domino 5 - View Functions ----------------------------------------------

(defn timer-comp [timer]
  (let [interval-handle (atom nil)]
    (r/create-class
      {:component-did-mount
       (fn [_]
         (->> (js/setInterval #(rf/dispatch [:timer/update-current-time (:id timer)]) 1000)
              (reset! interval-handle)))

       :component-will-unmount
       (fn [_]
         (js/clearInterval @interval-handle))

       :reagent-render
       (fn [timer]
         [:<>
          [:div.example-clock
           {:style {:color (:color timer)}}
           (:timer/display-value timer)]
          [:div.color-input
           [:input {:type "text"
                    :value (:color timer)
                    :on-change #(rf/dispatch [:timer/change-color (:id timer) (-> % .-target .-value)])}]]
          [:button {:on-click #(rf/dispatch [:timer/delete (:id timer)])} "Delete timer"]])})))


(defn timer-list-item-comp [timer-id]
  (let [timer @(rf/subscribe [:timer timer-id])]
    [:li [timer-comp timer]]))

(defn timer-list-comp []
  (let [timers @(rf/subscribe [:timers])]
    [:ul (for [index (range (count timers))
               :let [timer-id (-> timers (nth index))]]
           ^{:key timer-id} [timer-list-item-comp timer-id])]))

(defn debug-comp []
  [:pre (pp-str @(rf/subscribe [:debug/db]))])

(defn ui []
  [:div
   [:h1 "Hello timers"]
   [:button {:on-click #(rf/dispatch [:timer/create])} "Create timer"]
   [timer-list-comp]
   [debug-comp]])

;; -- Entry Point -------------------------------------------------------------

(defn render []
  (dom/render [ui] (js/document.getElementById "app")))

(defn ^:dev/after-load after-load-hook []
  (rf/clear-subscription-cache!)
  (render))

(defn run []
  (rf/dispatch-sync [:initialize-app-db])
  (rf/dispatch-sync [:timer/create])
  (render))
