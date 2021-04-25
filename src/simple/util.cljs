(ns simple.util
  (:require [cljs.pprint :as pp]))

(defn pp-str
  "Pretty print a data into a string."
  [data]
  (with-out-str (pp/pprint data)))

(defn with-id [entity id]
  (assoc entity :id id))

(def last-entity-id (atom -1))

(defn ensure-id [entity]
  (if (contains? entity :id)
    entity
    (let [id (swap! last-entity-id inc)]
      (with-id entity id))))
