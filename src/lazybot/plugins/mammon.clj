;;; Written by Paul L. Snyder <paul@pataprogramming.com>
;;; Built from a core of code borrowed from lazybot.plugins.karma
;;; by Michael D. Ivey <ivey@gweezlebur.com>
;;; Licensed under the EPL

(ns lazybot.plugins.mammon
  (:use [lazybot registry info]
        [lazybot.plugins.login :only [when-privs]]
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

(defn fmt-currency
  [amount channel]
  (str "\u20b1" (format "%.2f" amount)))

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
  [com-m nick server channel price]
  (let [attrs (key-attrs nick server channel)]
    (if (= price 0)
      (destroy! :price attrs)
      (update! :price attrs (assoc attrs :price price)))))

(defn- delist-price
  [com-m nick server channel]
  (set-price com-m nick server channel 0))

(defn- get-price
  [{:keys [com] :as com-m} nick server channel]
  (let [user-map (fetch-one :price
                            :where (key-attrs nick server channel))
        db-price (get user-map :price 0.0)
        _ (println "Got price " db-price " for " nick)
        price    (if (= db-price 0.0)
                   (let [ipo-price (rand-price)]
                     (set-price com-m nick (:server @com) channel ipo-price)
                     (send-message com-m
                                   (str "New IPO: " nick " at "
                                        (fmt-currency ipo-price channel)))
                     ipo-price)
                   db-price)]
    price))

(defn- get-market
  [server channel]
  (let [price-maps (fetch :price
                          :where (keyed [server channel]))]
    (println "got" price-maps "for server" server "channel" channel)
    price-maps))

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
  (let [msg
        (cond
         (.equalsIgnoreCase nick stock)
         (str nick ": Buying shares in yourself is insider trading.")
         :else
         (do
           (set-shares nick (:server @com) channel stock new-shares)
           (str nick ": You now own " new-shares " shares of \u00a7" stock
                " at " (fmt-currency (get-price com-m stock (:server @com) channel) channel) ".")))]
    (send-message com-m msg)))

(defn- fmt-share
  ;; FIXME: Fix calling convention
  [{:keys [stock shares channel] :as com-m} server & fmt-price?]
  (let [base (str shares " shares of \u00a7" stock)]
    (if fmt-price?
      (let [price (get-price com-m stock server channel)]
        (str base " at " (fmt-currency price channel)))
      base)))

(defn- fmt-owner
  [{:keys [nick shares]}]
  (str "\u00a7" nick " (" shares ")"))

(defn shares-fn
  "Create a plugin command function that applies f to the stock owned by the user specified in args."
  [f]
  (fn [{:keys [nick com channel args] :as com-m}]
    (let [stock (first args)
          shares (get-shares nick (:server @com) channel stock)
          new-shares (f shares)]
      (let [old-price   (get-price com-m stock (:server @com) channel)
            price-delta (+ 0.01 (/ (rand-int 100) 100.0))]
        (cond
         (< new-shares shares) (set-price com-m stock (:server @com) channel
                                          (max 0.01 (- old-price price-delta)))
         (> new-shares shares) (set-price com-m stock (:server @com) channel
                                          (max 0.01 (+ old-price price-delta)))))
      (change-shares stock new-shares com-m))))

(defn total-value
  [com-m stocks]
  (reduce + (map #(* (:shares %)
                     (get-price com-m (:stock %) (:server %) (:channel %))) stocks)))

(def print-portfolio
  (fn [{:keys [nick com bot channel args] :as com-m}]
    (let [server (:server @com)]
      (send-message
       com-m
       (let [stocks (get-stocks nick server channel)]
         (if (> (count stocks) 0)
           (str
            nick ": You own "
            (oxford-list (map #(fmt-share % server true) stocks))
            " (totalling "
            (fmt-currency (total-value com-m stocks) channel) ").")
           (str nick " does not own any shares.")))))))

(def print-price
  (fn [{:keys [nick com channel args] :as com-m}]
    (let [stock (first args)]
      (if stock
        (send-message
         com-m
         (if-let [price (get-price com-m stock (:server @com) channel)]
           (str nick ": \u00a7" stock " is currently priced at "
                (fmt-currency price channel) ".")
           (str nick ": " stock " is not listed on the "
                channel " exchange.")))
        (send-message
         com-m
         (str nick ": No stock symbol specified."))))))


(def print-shares
  (fn [{:keys [nick com channel args] :as com-m}]
    (let [stock (first args)]
      (if stock
        (send-message com-m
                      (let [shares (get-shares nick (:server @com) channel stock)
                            price  (get-price com-m stock (:server @com) channel)]
                        (if shares
                          (str nick ": You own " shares " shares of \u00a7" stock
                               " at " (fmt-currency price channel)
                               " (" (fmt-currency (* shares price) channel) " total).")
                          (str nick ": I have no record for shares of \u00a7" stock
                               " owned by " nick "."))))
        (print-portfolio com-m)))))


(def print-ownership
  (fn [{:keys [nick com bot channel args] :as com-m}]
    (let [stock (or (first args) nick)]
      (send-message
       com-m
       (let [owners (get-ownership nick (:server @com) channel stock)]
         (if (> (count owners) 0)
           ;; FIXME: Grammar case for single user
           (str nick ": The users who own shares in \u00a7" stock " are "
                (oxford-list
                 (map fmt-owner (reverse (sort-by :shares owners))))
                ".")
           (str nick ": No users own shares in \u00a7" stock ".")))))))

(def print-ticker
  (fn [{:keys [nick com bot channel args] :as com-m}]
    (let [prices (get-market (:server @com) channel)]
      (println (str "number returned: " (count prices)))
      (println (map #(fmt-currency (:price %) channel) prices))
      (send-message
       com-m
       (str "Current market values: "
            (s/join "; " (map #(str "\u00a7" (:nick %) ":"
                                    (fmt-currency (:price %) channel))
                              prices)))))))

(def buy (shares-fn #(+ % 100)))
(def sell (shares-fn #(- % 100)))

(def margin-order
  (fn [{:keys [nick] :as  com-m}]
    (send-message com-m (str nick ": You don't have a margin account!"))))

(def delist
  (fn [{:keys [nick com channel args] :as com-m}]
    (when-privs
     com-m :admin
     (let [stock (or (first args) nick)]
       (delist-price com-m stock (:server @com) channel)
       (send-message
        com-m
        (str nick ": " stock " has been delisted from the " channel " exchange."))))))


(defplugin
  (:cmd
   "Checks the shares of a stock that you own."
   #{"shares" "stock" "owned"} print-shares)
  (:cmd
   "Displays the market prices for all stocks."
   #{"ticker" "market"} print-ticker)
  (:cmd
   "Checks the current price of a stock."
   #{"price"} print-price)
  (:cmd
   "Checks all of the stocks that you own."
   #{"portfolio"} print-portfolio)
  (:cmd
   "Purchases shares in the stock you specify."
   #{"buy"} buy)
  (:cmd
   "Sell shares of the person you specify."
   #{"sell"} sell)
  (:cmd
   "Remove a symbol from the exchange. (Admin-only)"
   #{"delist"} delist)
  (:cmd
   "Issue a sell-short order. (Unimplemented)"
   #{"short"} margin-order)
  (:cmd
   "Shows the shares owned in the given stock."
   #{"ownership" "owners" "owner" "own"} print-ownership)
  (:indexes [[:server :channel :nick]]))
