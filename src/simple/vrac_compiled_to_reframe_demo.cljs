(ns simple.vrac-compiled-to-reframe-demo
  (:require [reagent.core :as r]
            [reagent.dom :as dom]
            [re-frame.core :as rf]
            [vrac.db :refer [Id]]
            [vrac.reframe :refer [with-id ensure-id
                                  follow-relation follow-relations from-path
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
          timers-path (follow-relations db nil [(Id. :timer-list) :timer-list/timers])
          timers (from-path db timers-path)
          updated-timers (conj timers (:vrac.db/id timer))]
      {:vrac.db/changes [(change-create timer)
                         (change-update timers-path updated-timers)]})))

(rf/reg-event-fx
  :timer/delete
  (fn [{:keys [db]} [_ timer-path]]
    (let [timer (from-path db timer-path)
          timers-path (follow-relations db nil [(Id. :timer-list) :timer-list/timers])
          timers (from-path db timers-path)
          updated-timers (into [] (remove #{(:vrac.db/id timer)}) timers)]
      {:vrac.db/changes [(change-delete timer-path)
                         (change-update timers-path updated-timers)]})))

(rf/reg-event-fx
 :timer/change-color
 (fn [_ [_ color-path new-color]]
   {:vrac.db/changes [(change-update color-path new-color)]}))

(rf/reg-event-fx
 :timer/update-current-time
 [(rf/inject-cofx :time/now)]
 (fn [{:keys [db time/now]} [_ timer-path]]
   (let [start-time-path (follow-relation db timer-path :timer/start-time)
         start-time (from-path db start-time-path)
         display-value-path (follow-relation db timer-path :timer/display-value)
         display-value (time-in-ms->display-value (- now start-time))]
     {:vrac.db/changes [(change-update display-value-path display-value)]})))


;; -- Domino 4 - Query  -------------------------------------------------------

(rf/reg-sub
  :debug/db
  (fn [db _]
    db))


;; -- Domino 5 - View Functions ----------------------------------------------

(defn timer-comp [timer-path]
  (let [interval-handle (atom nil)]
    (r/create-class
      {:component-did-mount
       (fn [_]
         (->> (js/setInterval #(rf/dispatch [:timer/update-current-time timer-path]) 1000)
              (reset! interval-handle)))

       :component-will-unmount
       (fn [_]
         (js/clearInterval @interval-handle))

       :reagent-render
       (fn [timer-path]
         (let [timer @(rf/subscribe [:vrac.db/from-path timer-path])
               color-path @(rf/subscribe [:vrac.db/follow-relation timer-path :color])]
           [:<>
            [:div.example-clock
             {:style {:color (:color timer)}}
             (:timer/display-value timer)]
            [:div.color-input
             [:input {:type "text"
                      :value (:color timer)
                      :on-change #(rf/dispatch [:timer/change-color color-path (-> % .-target .-value)])}]]
            [:button {:on-click #(rf/dispatch [:timer/delete timer-path])} "Delete timer"]]))})))


(defn timer-list-item-comp [timers-path index]
  (let [timer-path @(rf/subscribe [:vrac.db/follow-relation timers-path index])]
    [:li [timer-comp timer-path]]))

(defn timer-list-comp []
  (let [timers-path @(rf/subscribe [:vrac.db/follow-relations nil [(Id. :timer-list) :timer-list/timers]])
        timers @(rf/subscribe [:vrac.db/from-path timers-path])]
    [:ul (for [index (range (count timers))
               :let [timer-id (-> timers (nth index) :id)]]
           ^{:key timer-id} [timer-list-item-comp timers-path index])]))

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
