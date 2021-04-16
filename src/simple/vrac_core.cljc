(ns simple.vrac-core
  (:require [vrac.core :as v :refer [defc]]))

;; -- Domino 2 - Event Handlers -----------------------------------------------

;; Event handlers describe a change, but do not enact it.
(v/reg-event-handler
  '{;; An event handler registered on multiple events.
    :event {[:initialize color] {name "Nathalie"} ;; default value provided, could also be (:default-name nil)
            [:initialize color name] {}}

    ;; The logic of the event, expresses the outputs from the inputs.
    :logic (let [input-realtime    (:co-effect/realtime nil)
                 output-time       (:time nil)
                 output-time-color (:time-color nil)
                 output-user-name  (:user-name nil)]
             {output-time       (-> input-realtime
                                    .toTimeString
                                    (str/split " ")
                                    first)
              output-time-color color
              output-user-name  name})
    ;; alternative way to write it
    #_#_:logic {(:time nil)       (-> (:co-effect/realtime nil)
                                      .toTimeString
                                      (str/split " ")
                                      first)
                (:time-color nil) color
                (:user-name nil)  name}})

;; Example of an event handler which add a value to an existing one.
(v/reg-event-handler
  '{:event {[:add-to acc val] {}
            [:add-to-global val] {acc (:global-acc nil)}}
    :logic {acc (+ acc val)}})


;; usage:  (dispatch [:time-color-change 34562])
(v/reg-event-handler
  '{:event [:time-color-change new-color-value]
    :logic {(:time-color nil) new-color-value}})

;; usage:  (dispatch [:timer a-js-Date])
(v/reg-event-handler
  '{:event [:timer new-time]
    :logic {(:time nil) new-time}})

;; -- Domino 4 - Query  -------------------------------------------------------

;; Will be replaced by Pathom resolvers

;;(rf/reg-sub
;; :time
;; (fn [db _]     ;; db is current app state. 2nd unused param is query vector
;;   (:time db))) ;; return a query computation over the application state
;;
;;(rf/reg-sub
;; :time-color
;; (fn [db _]
;;   (:time-color db)))


;; -- Domino 5 - View Functions ----------------------------------------------

'(defc clock []
   [:div.example-clock
    {:style {:color (:time-color nil)}}
    (:time nil)])

'(defc color-input []
   [:div.color-input
    "Time color: "
    [:input {:type "text"
             :value (:time-color nil)
             :on-change [:time-color-change (-> %evt .-target .-value)]}]]) ;; <--- %evt is a reserved word, related to the closest :on-xxx ancestor.

'(defc ui []
   [:div
    [:h1 "Hello " (:user-name nil) ", it is now"]
    [clock]
    [color-input]])


;; -- Domino 1 - Event Dispatch -----------------------------------------------

(defn dispatch-timer-event []
  (let [now (-> (js/Date.)
                .toTimeString
                (str/split " ")
                first)]
    (v/dispatch [:timer now])))

;; Call the dispatching function every second.
(defn ^:export init []
  ;; Setup a timer
  (js/setInterval dispatch-timer-event 1000)

  ;; Initialize the DB.
  (v/dispatch [:initialize "#f88"]))
