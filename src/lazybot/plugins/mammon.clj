;;; Written by Paul L. Snyder <paul@pataprogramming.com>
;;; Built from a core of code borrowed from lazybot.plugins.karma
;;; by Michael D. Ivey <ivey@gweezlebur.com>
;;; Licensed under the EPL

(ns lazybot.plugins.mammon
  (:use [lazybot registry info]
        [useful.map :only [keyed]]
        [somnium.congomongo :only [fetch fetch-one insert! update! destroy!]])
  (:require[clojure.string :as s])
  (:import (java.util.concurrent Executors ScheduledExecutorService TimeUnit)))

(defn oxford-list [s]
  (case (count s)
    0    ""
    1    (str (first s))
    2    (str (first s) " and " (second s))
    (str (s/join ", " (butlast s)) " and " (last s))))

(defn- key-attrs
  ([nick server channel]
     (let [nick  (.toLowerCase nick)]
       (keyed [nick server channel])))
  ([nick server channel stock]
     (let [nick  (.toLowerCase nick)
           stock (.toLowerCase stock)]
       (keyed [nick server channel stock ]))))

(defn- set-shares
  [nick server channel stock shares]
  (let [attrs (key-attrs nick server channel stock)]
    (if (= shares 0)
      (destroy! :shares attrs)
      (update! :shares attrs (assoc attrs :shares shares)))))

(defn- rand-price
  []
  (+ 1 (/ (rand-int 900) 100.0)))

(defn- set-price
  [nick server channel price]
  (let [attrs (key-attrs nick server channel)]
    (if (= price 0)
      (destroy! :price attrs)
      (update! :price attrs (assoc attrs :price price)))))

(defn- get-price
  [nick server channel]
  (let [user-map (fetch-one :price
                            :where (key-attrs nick server channel))
        price    (get user-map :price 0)]
    (if (= price 0)
      (let [ipo-price (rand-price)]
        (set-price nick server channel ipo-price)
        ipo-price)
      price)))

(defn- get-ownership
  [nick server channel stock]
  (let [stock (.toLowerCase stock)
        owner-maps (fetch :shares
                        :where (keyed [server channel stock]))]
    (println "got" owner-maps "for server" server "channel" channel
             "stock" stock)
    owner-maps))

(defn- get-stocks
  [nick server channel]
  (let [stock-maps (fetch :shares
                          :where (key-attrs nick server channel))]
    stock-maps))

(defn- get-shares
  [nick server channel stock]
  (let [stock-map (fetch-one :shares
                            :where (key-attrs nick server channel stock))]
    (get stock-map :shares 0)))

(def limit (ref {}))

(let [scheduler (Executors/newScheduledThreadPool 1)]
  (defn schedule [^Runnable task]
    (.schedule scheduler task
               5 TimeUnit/MINUTES)))

(defn- change-shares
  [stock new-shares {:keys [^String nick com bot channel] :as com-m}]
  (let [new-shares (if (< new-shares 0) 0 new-shares)
        msg
        (cond
         (.equalsIgnoreCase nick stock) "Buying shares in yourself is insider trading."
         :else (do
                 (set-shares nick (:server @com) channel stock new-shares)
                 (str nick " now owns " new-shares " shares of " stock ".")))]
    (send-message com-m msg)))

(defn- fmt-share
  ;; FIXME: Fix calling convention
  [{:keys [stock shares channel]} server & fmt-price?]
  (let [base (str shares " shares of " stock)]
    (if fmt-price?
      (let [price (get-price stock server channel)]
        (str base " at " (fmt-currency price channel)))
      base)))

(defn- fmt-owner
  [{:keys [nick shares]}]
  (str nick " (" shares ")"))

(defn shares-fn
  "Create a plugin command function that applies f to the stock owned bythe user specified in args."
  [f]
  (fn [{:keys [nick com channel args] :as com-m}]
    (let [stock (first args)
          shares (get-shares nick (:server @com) channel stock)
          new-shares (f shares)]
      (change-shares stock new-shares com-m))))

(defn fmt-currency
  [amount channel]
  (str "\u20b1" (format "%.2f" amount)))

(defn total-value
  [stocks]
  (reduce + (map #(* (:shares %)
                     (get-price (:stock %) (:server %) (:channel %)))
                 stocks)))

(def print-price
  (fn [{:keys [nick com channel args] :as com-m}]
    (let [stock (first args)]
      (if stock
        (send-message com-m
                      (if-let [price (get-price stock (:server @com) channel)]
                        (str stock " is currently priced at "
                             (fmt-currency price channel) ".")
                        (str stock " is not listed on the " channel " exchange.")))
        (send-message com-m
                      (str "No stock symbol specified."))))))


(def print-shares
  (fn [{:keys [nick com channel args] :as com-m}]
    (let [stock (first args)]
      (if stock
        (send-message com-m
                      (let [shares (get-shares nick (:server @com) channel stock)
                            price  (get-price stock (:server @com) channel)]
                        (if shares
                          (str nick " owns " shares " shares of " stock
                               " at " (fmt-currency price channel)
                               " (" (fmt-currency (* shares price) channel) " total).")
                          (str "I have no record for shares of " stock
                               " owned by " nick "."))))
        (print-portfolio com-m)))))


(def print-portfolio
  (fn [{:keys [nick com bot channel args] :as com-m}]
    (let [server (:server @com)]
      (send-message
       com-m
       (let [stocks (get-stocks nick server channel)]
         (if (> (count stocks) 0)
           (str
            nick " owns "
            (oxford-list (map #(fmt-share % server true) stocks))
            " (totalling "
            (fmt-currency (total-value stocks) channel) ").")
           (str nick " does not own any shares.")))))))


(def print-ownership
  (fn [{:keys [nick com bot channel args] :as com-m}]
    (let [stock (or (first args) nick)]
      (send-message
       com-m
       (let [owners (get-ownership nick (:server @com) channel stock)]
         (if (> (count owners) 0)
           (str "The users who own shares in " stock " are "
                (oxford-list
                 (map fmt-owner (reverse (sort-by :shares owners))))
                ".")
           (str "No users own shares in " stock ".")))))))

(def buy (shares-fn #(+ % 100)))
(def sell (shares-fn #(- % 100)))

(defplugin
  (:cmd
   "Checks the shares of a stock that you own."
   #{"shares" "stock" "owned"} print-shares)
  (:cmd
   "Checks the current price of a stock."
   #{"price"} print-price)
  (:cmd
   "Checks all of the stocks that you own."
   #{"portfolio"} print-portfolio)
  (:cmd
   "Increases the karma of the person you specify."
   #{"buy"} buy)
  (:cmd
   "Decreases the karma of the person you specify."
   #{"sell"} sell)
  (:cmd
   "Shows the shares owned in the given nick."
   #{"ownership" "owners" "owner" "own"} print-ownership)
  (:indexes [[:server :channel :nick]]))
