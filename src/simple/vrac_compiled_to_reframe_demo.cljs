(ns simple.vrac-compiled-to-reframe-demo
  (:require [reagent.core :as r]
            [reagent.dom :as dom]
            [re-frame.core :as rf]
            [vrac.db :refer [Id]]
            [vrac.reframe :refer [with-id ensure-id
                                  follow-relation follow-relations from-ref
                                  change-create change-update change-remove change-delete]]
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
          timers-ref (follow-relations nil [(Id. :timer-list) :timer-list/timers])
          timers (from-ref db timers-ref)
          updated-timers (conj timers (:vrac.db/id timer))]
      {:vrac.db/changes [(change-create timer)
                         (change-update timers-ref updated-timers)]})))

(rf/reg-event-fx
  :timer/delete
  (fn [_ [_ timer-ref]]
    {:vrac.db/changes [(change-delete timer-ref)
                       (change-remove timer-ref)]}))


(rf/reg-event-fx
 :timer/change-color
 (fn [_ [_ color-ref new-color]]
   {:vrac.db/changes [(change-update color-ref new-color)]}))

(rf/reg-event-fx
 :timer/update-current-time
 [(rf/inject-cofx :time/now)]
 (fn [{:keys [db time/now]} [_ timer-ref]]
   (let [start-time-ref (follow-relation timer-ref :timer/start-time)
         start-time (from-ref db start-time-ref)
         display-value-ref (follow-relation timer-ref :timer/display-value)
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
         (let [timer-cref @(rf/subscribe [:vrac.db/canonical-ref timer-ref])]
           (->> (js/setInterval #(rf/dispatch [:timer/update-current-time timer-cref]) 1000)
                (reset! interval-handle))))

       :component-will-unmount
       (fn [_]
         (js/clearInterval @interval-handle))

       :reagent-render
       (fn [timer-ref]
         (let [color-ref (follow-relation timer-ref :color)
               color @(rf/subscribe [:vrac.db/from-ref color-ref])
               display-value-ref (follow-relation timer-ref :timer/display-value)
               display-value @(rf/subscribe [:vrac.db/from-ref display-value-ref])]
           [:<>
            [:div.example-clock
             {:style {:color color}}
             display-value]
            [:div.color-input
             [:input {:type "text"
                      :value color
                      :on-change #(rf/dispatch [:timer/change-color color-ref (-> % .-target .-value)])}]]
            [:button {:on-click #(rf/dispatch [:timer/delete timer-ref])} "Delete timer"]]))})))

(defn timer-list-comp []
  (let [timers-ref (follow-relations nil [(Id. :timer-list) :timer-list/timers])
        timers @(rf/subscribe [:vrac.db/from-ref timers-ref])]
    [:ul (for [index (range (count timers))]
           (let [timer-id (-> timers (nth index) :id)
                 timer-ref (follow-relation timers-ref index)]
             ^{:key timer-id} [:li [timer-comp timer-ref]]))]))

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
