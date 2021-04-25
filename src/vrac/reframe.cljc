(ns vrac.reframe
  (:require [re-frame.core :as rf]
            [re-frame.db :as rf-db]
            [vrac.db :refer [Id]]))

;; -- Setup - change helpers --------------------------------------------------

(defn with-id [entity id]
  (assoc entity :vrac.db/id (Id. id)))

(def last-entity-id (atom -1))

(defn ensure-id [entity]
  (if (instance? Id (:vrac.db/id entity))
    entity
    (let [id (swap! last-entity-id inc)]
      (with-id entity id))))

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
  :initialize-vrac-db
  (fn [_ _]
    {;; Vrac is oblivious to "entity types", it stores all entities homogeneously.
     :vrac.db.entity/by-id {}}))

;; -- Domino 4 - Query  -------------------------------------------------------

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
