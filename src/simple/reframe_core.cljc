(ns simple.reframe-core
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [reagent.dom :as dom]
            [re-frame.core :as rf]
            [re-frame.db :as rf-db]))

;; -- Setup - change helpers --------------------------------------------------

(def last-entity-id (atom -1))

(defn make-entity [entity]
  (let [id (swap! last-entity-id inc)]
    (assoc entity :vrac.db/id id)))

(defn change-create [entity]
  [:vrac.db.change/create entity])

(defn change-update [path value]
  [:vrac.db.change/update path value])

(defn change-delete [path]
  [:vrac.db.change/delete path])

;; Fix me: This does not work for indexes. Create an Id record instead.
(def relations-toward-entity
  #{})

;; "relation" is a (possibly namespaced) keyword
(defn follow-relation [db path relation]
  (if (contains? relations-toward-entity relation)
    (get-in db (conj path relation))
    (conj path relation)))


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

;; Fix me: use tmp ids inside event handlers to make them idempotent.
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
    :vrac.db.entity/by-id {}}))

(defn time-in-ms->display-value [ms]
  (-> (js/Date. ms)
      .toISOString
      (subs 11 19)))

(rf/reg-event-fx
  :timer/create
  [(rf/inject-cofx :time/now)]
  (fn [{:keys [time/now]} _]
    {:vrac.db/changes [(change-create (make-entity {:timer/start-time    now
                                                    :timer/display-value (time-in-ms->display-value 0)
                                                    :color               "#888"}))]}))

(rf/reg-event-fx
  :timer/delete
  (fn [_ [_ timer-path]]
    {:vrac.db/changes [(change-delete timer-path)]}))

(rf/reg-event-fx
 :timer/change-color
 (fn [_ [_ color-path new-color]]
   {:vrac.db/changes [(change-update color-path new-color)]}))

(rf/reg-event-fx
 :timer/update-current-time
 [(rf/inject-cofx :time/now)]
 (fn [{:keys [db time/now]} [_ timer-path]]
   (let [start-time-path (follow-relation db timer-path :timer/start-time)
         start-time (get-in db start-time-path)
         display-value-path (follow-relation db timer-path :timer/display-value)
         display-value (time-in-ms->display-value (- now start-time))]
     {:vrac.db/changes [(change-update display-value-path display-value)]})))


;; -- Domino 4 - Query  -------------------------------------------------------

(rf/reg-sub
 :debug/db
 (fn [db _]
   db))

(rf/reg-sub
 :vrac.db.entity/all-entity-paths
 (fn [db _]
   (mapv (fn [id] [:vrac.db.entity/by-id id])
         (-> db :vrac.db.entity/by-id keys))))

(rf/reg-sub
  :vrac.db/from-path
  (fn [db [_ path]]
    (get-in db path)))

(rf/reg-sub
  :vrac.db/follow-relation
  (fn [db [_ path relation]]
    (follow-relation db path relation)))


;; -- Domino 5 - View Functions ----------------------------------------------

(defn timer-comp [timer-path]
  (let [interval-handle (atom nil)]
    (r/create-class
      {:component-did-mount
       (fn [this]
         (->> (js/setInterval #(rf/dispatch [:timer/update-current-time timer-path]) 1000)
              (reset! interval-handle)))

       :component-will-unmount
       (fn [this]
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


(defn timer-list-comp []
  ;; Fix me: all the entities are not timers, we need a timer list.
  (let [timer-paths @(rf/subscribe [:vrac.db.entity/all-entity-paths])]
    [:ul
     (for [timer-path timer-paths]
       ^{:key (last timer-path)} [:li [timer-comp timer-path]])]))

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
