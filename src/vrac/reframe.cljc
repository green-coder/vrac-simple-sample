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

(defn change-update [ref value]
  [:vrac.db.change/update ref value])

(defn change-delete [ref]
  [:vrac.db.change/delete ref])

;; "relation" can be anything that goes as the key in `(get obj key)`.
(defn follow-relation [db ref relation]
  (loop [ref (if (nil? ref)
               [:vrac.db.entity/by-id relation]
               (conj ref relation))]
    (let [val (get-in db ref)]
      (if (instance? Id val)
        (recur [:vrac.db.entity/by-id val])
        ref))))

(defn follow-relations [db ref relations]
  (reduce (partial follow-relation db) ref relations))

(defn from-ref [db ref]
  (get-in db ref))


;; -- Setup - effects ---------------------------------------------------------

(rf/reg-fx
  :vrac.db/changes
  (fn [changes]
    (let [create-fn (fn [db [_ entity]]
                      (update db :vrac.db.entity/by-id
                              assoc (:vrac.db/id entity) entity))
          update-fn (fn [db [_ ref value]]
                      (assoc-in db ref value))
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
  :vrac.db/from-ref
  (fn [db [_ ref]]
    (from-ref db ref)))

(rf/reg-sub
  :vrac.db/follow-relation
  (fn [db [_ ref relation]]
    (follow-relation db ref relation)))

(rf/reg-sub
  :vrac.db/follow-relations
  (fn [db [_ ref relations]]
    (follow-relations db ref relations)))
