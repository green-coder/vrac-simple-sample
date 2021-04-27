(ns simple.vrac-compiled-to-reframe-demo
  (:require [reagent.core :as r]
            [reagent.dom :as dom]
            [re-frame.core :as rf]
            [vrac.db :refer [Id]]
            [vrac.reframe :refer [with-id ensure-id
                                  follow-relation follow-relations from-ref
                                  change-create change-update change-delete]]
            [simple.util :refer [pp-str]]))

;; -- Setup - coeffects -------------------------------------------------------

(rf/reg-cofx
  :time/now
  (fn [coeffects]
    (assoc coeffects :time/now (js/Date.))))


;; -- Domino 2 - Event Handlers -----------------------------------------------

(rf/reg-event-fx
 :initialize-app-db
 (fn [_ _]
   {:vrac.db/changes [(change-create (-> {:timer-list/timers []}
                                         (with-id :timer-list)))]}))

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
          ;; TODO: won't work when multiple timer-list can exist.
          ;; TODO: Introduce :timer/timer-lists and computed data.
          timers-ref (follow-relations db nil [(Id. :timer-list) :timer-list/timers])
          timers (from-ref db timers-ref)
          updated-timers (conj timers (:vrac.db/id timer))]
      {:vrac.db/changes [(change-create timer)
                         (change-update timers-ref updated-timers)]})))

(rf/reg-event-fx
  :timer/delete
  (fn [{:keys [db]} [_ timer-ref]]
    (let [timer (from-ref db timer-ref)
          timers-ref (follow-relations db nil [(Id. :timer-list) :timer-list/timers])
          timers (from-ref db timers-ref)
          updated-timers (into [] (remove #{(:vrac.db/id timer)}) timers)]
      {:vrac.db/changes [(change-delete timer-ref)
                         (change-update timers-ref updated-timers)]})))

(rf/reg-event-fx
 :timer/change-color
 (fn [_ [_ color-ref new-color]]
   {:vrac.db/changes [(change-update color-ref new-color)]}))

(rf/reg-event-fx
 :timer/update-current-time
 [(rf/inject-cofx :time/now)]
 (fn [{:keys [db time/now]} [_ timer-ref]]
   (let [start-time-ref (follow-relation db timer-ref :timer/start-time)
         start-time (from-ref db start-time-ref)
         display-value-ref (follow-relation db timer-ref :timer/display-value)
         display-value (time-in-ms->display-value (- now start-time))]
     {:vrac.db/changes [(change-update display-value-ref display-value)]})))


;; -- Domino 4 - Query  -------------------------------------------------------

(rf/reg-sub
  :debug/db
  (fn [db _]
    db))


;; -- Domino 5 - View Functions ----------------------------------------------

(defn timer-comp [timer-ref]
  (let [interval-handle (atom nil)]
    (r/create-class
      {:component-did-mount
       (fn [_]
         (->> (js/setInterval #(rf/dispatch [:timer/update-current-time timer-ref]) 1000)
              (reset! interval-handle)))

       :component-will-unmount
       (fn [_]
         (js/clearInterval @interval-handle))

       :reagent-render
       (fn [timer-ref]
         (let [timer @(rf/subscribe [:vrac.db/from-ref timer-ref])
               color-ref @(rf/subscribe [:vrac.db/follow-relation timer-ref :color])]
           [:<>
            [:div.example-clock
             {:style {:color (:color timer)}}
             (:timer/display-value timer)]
            [:div.color-input
             [:input {:type "text"
                      :value (:color timer)
                      :on-change #(rf/dispatch [:timer/change-color color-ref (-> % .-target .-value)])}]]
            [:button {:on-click #(rf/dispatch [:timer/delete timer-ref])} "Delete timer"]]))})))


(defn timer-list-item-comp [timers-ref index]
  (let [timer-ref @(rf/subscribe [:vrac.db/follow-relation timers-ref index])]
    [:li [timer-comp timer-ref]]))

(defn timer-list-comp []
  (let [timers-ref @(rf/subscribe [:vrac.db/follow-relations nil [(Id. :timer-list) :timer-list/timers]])
        timers @(rf/subscribe [:vrac.db/from-ref timers-ref])]
    [:ul (for [index (range (count timers))
               :let [timer-id (-> timers (nth index) :id)]]
           ^{:key timer-id} [timer-list-item-comp timers-ref index])]))

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
  (rf/dispatch-sync [:initialize-vrac-db])
  (rf/dispatch-sync [:initialize-app-db])
  (rf/dispatch-sync [:timer/create])
  (render))
