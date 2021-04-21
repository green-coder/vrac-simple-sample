(ns vrac.brainstorming-event-handler)

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
