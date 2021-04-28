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

(defn change-remove [ref]
  [:vrac.db.change/remove ref])

(defn change-delete [ref]
  [:vrac.db.change/delete ref])

;; "relation" can be anything that goes as the key in `(get obj key)`.
(defn follow-relation [ref relation]
  (conj (or ref []) relation))

(defn follow-relations [ref relations]
  (into (or ref []) relations))

(defn from-ref [db ref]
  (let [entity-by-id (:vrac.db.entity/by-id db)]
    (loop [s (seq ref)
           val entity-by-id]
      (if s
        (let [val (get val (first s))]
          (recur (next s)
                 (cond-> val
                   (instance? Id val) entity-by-id)))
        val))))

(defn- canonical-ref* [entity-by-id ref]
  (loop [path []
         s (seq ref)
         val entity-by-id]
    (if s
      (let [relation (first s)
            val (get val relation)]
        (if (instance? Id val)
          (recur [val]
                 (next s)
                 (entity-by-id val))
          (recur (conj path relation)
                 (next s)
                 val)))
      path)))

(defn canonical-ref [db ref]
  (canonical-ref* (:vrac.db.entity/by-id db) ref))


;; -- Setup - effects ---------------------------------------------------------

(defn- safe-update-in
  "Same as update-in, but also supports empty paths."
  [m path f & args]
  (if (seq path)
    (apply update-in m path f args)
    (apply f m args)))


;; Works for vectors and maps.
(defn- dissoc-in [data path]
  (safe-update-in data (butlast path)
                  (fn [container]
                    (assert (or (vector? container)
                                (map? container)) "dissoc-in only works on vectors and maps.")
                    (let [k (last path)]
                      (cond
                        (vector? container)
                        (into (subvec container 0 k)
                              (subvec container (inc k)))

                        :else
                        (dissoc container k))))))

;; TODO: (Problem) the order of the creations, updates and deletes inside a set of changes matters.
;; TODO: (Solution) change all references to canonical before processing any of the changes.
(rf/reg-fx
  :vrac.db/changes
  (fn [changes]
    (let [create-fn (fn [entity-by-id [_ entity]]
                      (assoc entity-by-id (:vrac.db/id entity) entity))
          update-fn (fn [entity-by-id [_ ref value]]
                      (let [cref (canonical-ref* entity-by-id ref)]
                        (assoc-in entity-by-id cref value)))
          remove-fn (fn [entity-by-id [_ ref]]
                      ;; Don't "canonical" last element of the ref
                      (let [parent-cref (canonical-ref* entity-by-id (butlast ref))
                            cref (follow-relation parent-cref (last ref))]
                        (dissoc-in entity-by-id cref)))
          delete-fn (fn [entity-by-id [_ ref]]
                      (let [cref (canonical-ref* entity-by-id ref)]
                        (dissoc-in entity-by-id cref)))]
      (swap! rf-db/app-db
             update :vrac.db.entity/by-id
             (fn [entity-by-id]
               (reduce (fn [entity-by-id change]
                         (let [f ({:vrac.db.change/create create-fn
                                   :vrac.db.change/update update-fn
                                   :vrac.db.change/remove remove-fn
                                   :vrac.db.change/delete delete-fn} (first change))]
                           (f entity-by-id change)))
                       entity-by-id
                       changes))))))


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
  :vrac.db/canonical-ref
  (fn [db [_ ref]]
    (canonical-ref db ref)))
