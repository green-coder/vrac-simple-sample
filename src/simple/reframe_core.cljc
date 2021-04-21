(ns simple.reframe-core
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [reagent.dom :as dom]
            [re-frame.core :as rf]
            [re-frame.db :as rf-db]
            [vrac.db :refer [Id]]))

;; -- Setup - change helpers --------------------------------------------------

(def last-entity-id (atom -1))

(defn ensure-id [entity]
  (if (instance? Id (:vrac.db/id entity))
    entity
    (let [id (swap! last-entity-id inc)]
      (assoc entity :vrac.db/id (Id. id)))))

(defn change-create [entity]
  [:vrac.db.change/create entity])

(defn change-update [path value]
  [:vrac.db.change/update path value])

(defn change-delete [path]
  [:vrac.db.change/delete path])

;; "relation" can be anything that goes as the key in `(get obj key)`.
(defn follow-relation [db path relation]
  (loop [path (if (nil? path)
                [:vrac.db.entity/by-id relation]
                (conj path relation))]
    (let [val (get-in db path)]
      (if (instance? Id val)
        (recur [:vrac.db.entity/by-id val])
        path))))

(defn follow-relations [db path relations]
  (reduce (partial follow-relation db) path relations))

(defn from-path [db path]
  (get-in db path))


;; -- Setup - coeffects -------------------------------------------------------

(rf/reg-cofx
  :time/now
  (fn [coeffects]
    (assoc coeffects :time/now (js/Date.))))


;; -- Setup - effects ---------------------------------------------------------

(rf/reg-fx
  :vrac.db/changes
  (fn [changes]
    (let [create-fn (fn [db [_ entity]]
                      (update db :vrac.db.entity/by-id
                              assoc (:vrac.db/id entity) entity))
          update-fn (fn [db [_ path value]]
                      (assoc-in db path value))
          delete-fn (fn [db [_ [_ id]]]
                      (update db :vrac.db.entity/by-id
                              dissoc id))
          {creates :vrac.db.change/create
           updates :vrac.db.change/update
           deletes :vrac.db.change/delete} (group-by first changes)]
      (swap! rf-db/app-db (fn [db]
                            (as-> db xxx
                              (reduce create-fn xxx creates)
                              (reduce update-fn xxx updates)
                              (reduce delete-fn xxx deletes)))))))


;; -- Setup - interceptors ----------------------------------------------------

;; TODO: use tmp ids inside event handlers to make them idempotent.
#_(defn inject-new-ids [nb-ids]
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
   {;; Vrac is oblivious to "entity types", it stores all entities homogeneously.
    :vrac.db.entity/by-id {(Id. :timer-list) {:timer-list/timers []}}}))

(defn time-in-ms->display-value [ms]
  (-> (js/Date. ms)
      .toISOString
      (subs 11 19)))

(rf/reg-event-fx
  :timer/create
  [(rf/inject-cofx :time/now)]
  (fn [{:keys [db time/now]} _]
    (let [timer (ensure-id {:timer/start-time    now
                            :timer/display-value (time-in-ms->display-value 0)
                            :color               "#888"})
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

(rf/reg-sub
  :vrac.db/from-path
  (fn [db [_ path]]
    (from-path db path)))

(rf/reg-sub
  :vrac.db/follow-relation
  (fn [db [_ path relation]]
    (follow-relation db path relation)))

(rf/reg-sub
  :vrac.db/follow-relations
  (fn [db [_ path relations]]
    (follow-relations db path relations)))


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
    [:ul (for [index (range (count timers))]
           ^{:key index} [timer-list-item-comp timers-path index])]))

(defn debug-comp []
  [:pre (with-out-str (cljs.pprint/pprint @(rf/subscribe [:debug/db])))])

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
