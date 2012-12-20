(ns nyhub.core
  (:require [tentacles.users :as users])
  (:import java.util.Date))

(def options {:per-page 100})

(defonce users (atom {}))

(def user-keys #{:name :avatar_url :bio :location :created_at
                 :login :email :type :hireable :blog :company})

;;TODO: walk multiple pages
(defn github [f & args]
  (let [result (apply f (concat args [options]))]
    (if-let [msg (:message result)]
      (throw (Exception. msg))
      result)))

(defn fetch-user [login]
  (assoc (select-keys (github users/user login) user-keys)
         :followers (set (map :login (github users/followers login)))
         :following (set (map :login (github users/following login)))
         :as-of (Date.)))

(def ttl 86400000) ; 24 hours

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
      (println login (count queue))
      (let [user (fresh-user login)
            related (apply concat ((juxt :followers :following) user))]
        (if (= depth max-depth)
          (recur (pop queue))
          (recur (into (pop queue)
                       (map vector related (repeat (inc depth))))))))))

(comment

  (fresh-user "brandonbloom")

  (walk "brandonbloom" 2)

  (count @users)

  (spit "users.edn" @users)

  (def users
    (-> "/Users/brandon/Projects/nyhub/users.edn" slurp read-string atom))

  ;; Find relevant New Yorkers
  (->> @users vals
       (filter (fn [{:keys [location]}]
                 (when location
                   (re-find #"(?i)(\bnyc?\b|new york)" location))))
       (map (juxt #(count (:followers %)) :login :location))
       sort
       pprint)

  ;; Eventually run this to clean up pagination errors
  (apply swap! users dissoc #(->> % vals
                                  (filter (fn [{:keys [followers]}]
                                            (= 100 (count followers))))
                                  (map :login)))
)
