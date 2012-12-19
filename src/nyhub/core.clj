(ns nyhub.core
  (:require [tentacles.users :as users])
  (:import java.util.Date))

(def options {})

(defonce users (atom {}))

(def user-keys #{:name :avatar_url :bio :location :created_at
                 :login :email :type :hireable :blog :company})

(defn fetch-user [login]
  (println "fetch: " login)
  (assoc (select-keys (users/user login options) user-keys)
         :followers (set (map :login (users/followers login options)))
         :following (set (map :login (users/following login options)))
         :as-of (Date.)))

(def ttl 43200000) ; 12 hours

(defn fresh? [user]
  (let [now (.getTime (Date.))
        expires (+ (.getTime (:as-of user)) ttl)]
    (< now expires)))

(defn fresh-user [login]
  (let [user (get @users login)]
    (if (and user (fresh? user))
      user
      (let [user (fetch-user login)]
        (swap! users assoc login user)
        user))))

(defn walk [start-login max-depth]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY [start-login 0])]
    (when-let [[login depth] (first queue)]
      (println "walk: " login)
      (let [user (fresh-user login)
            related (apply concat ((juxt :followers :following) user))]
        (if (= depth max-depth)
          (recur (pop queue))
          (recur (into (pop queue)
                       (map vector related (repeat (inc depth))))))))))

(comment

  (fresh-user "brandonbloom")

  (walk "brandonbloom" 1)

)

