(ns simple.vrac-demo
  (:require [vrac.core :as v :refer [defc]]))

;; -- Setup - coeffects -------------------------------------------------------

'(v/reg-cofx
   :time/now
   (js/Date.))


;; -- Domino 2 - Event Handlers -----------------------------------------------

'(v/reg-event-handler
   [:initialize-app-db]
   {:create (-> {:timer-list/timers []}
                (with-id :timer-list))})

(defn time-in-ms->display-value [ms]
  (-> (js/Date. ms)
      .toISOString
      (subs 11 19)))

'(v/reg-event-handler
   [:timer/create]
   (let [timer (-> {:timer/start-time    now
                    :timer/display-value (time-in-ms->display-value 0)
                    :color               "#888"}
                   ensure-id)
         timers (-> nil :timer-list :timer-list/timers)]
     {:create timer
      timers (conj timers timer)}))

'(v/reg-event-handler
   [:timer/delete timer
     {;; Remove the entity from the store
      :delete timer
      ;; Remove the link to the entity, assuming that the reference contains that link.
      :remove timer}])

'(v/reg-event-handler
   [:timer/change-color color new-color]
   {color new-color})

'(v/reg-event-handler
   [:timer/update-current-time timer]
   {(:timer/display-value timer) (time-in-ms->display-value (- (:time/now nil)
                                                               (:timer/start-time timer)))})

;; -- Domino 5 - View Functions ----------------------------------------------

'(defc timer-comp [timer]
   (let [interval-handle (state {:on-init (fn []
                                            (js/setInterval #(v/dispatch [:timer/update-current-time ^canonical-ref timer]) 1000))
                                 :on-delete (fn [val]
                                              (js/clearInterval val))})
         color (:color timer)]
     [:<>
      [:div.example-clock
       {:style {:color color}}
       (:timer/display-value timer)]
      [:div.color-input
       [:input {:type "text"
                :value color
                :on-change [:timer/change-color color (-> %evt .-target .-value)]}]]
      [:button {:on-click [:timer/delete timer]} "Delete timer"]]))

'(defc timer-list-comp []
   (let [timers (:timer-list/timers (:timer-list nil))]
     [:ul (for [timer timers]
            [:li [timer-comp timer]])]))

'(defc debug-comp []
   [:pre (pp-str (:debug/db nil))])

'(defc ui []
   [:div
    [:h1 "Hello timers"]
    [:button {:on-click [:timer/create]} "Create timer"]
    [timer-list-comp]
    [debug-comp]])


;; -- Entry Point -------------------------------------------------------------

(defn render []
  (dom/render [ui] (js/document.getElementById "app")))

#_(defn ^:dev/after-load after-load []
    (v/after-reload!)
    (render))

(defn run []
  (v/dispatch-sync [:initialize-vrac-db])
  (v/dispatch-sync [:initialize-app-db])
  (v/dispatch-sync [:timer/create])
  (render))
