;;; Written by Paul L. Snyder <paul@pataprogramming.com>
;;; Built from a core of code borrowed from lazybot.plugins.karma
;;; by Michael D. Ivey <ivey@gweezlebur.com>
;;; Licensed under the EPL

;;; Relies on 'seen' plugin from base lazybot distribution to
;;; validate that a symbol is a legitimate nick

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

(defn- validate-stock [nick server]
  (when-let [seen-map (fetch-one :seen :where {:nick (.toLowerCase nick)
                                               :server server})]
    seen-map))

(defn- fmt-currency
  [amount]
  (str "\u20b1" (format "%.2f" amount)))

(defn- key-attrs
  ([nick server]
     (let [nick  (.toLowerCase nick)]
       (keyed [nick server])))
  ([nick server stock]
     (let [nick  (.toLowerCase nick)
           stock (.toLowerCase stock)]
       (keyed [nick server stock ]))))

(defn- set-shares
  [nick server stock shares]
  (let [attrs (key-attrs nick server stock)]
    (if (= shares 0)
      (destroy! :shares attrs)
      (update! :shares attrs (assoc attrs :shares shares)))))

(defn- rand-price
  []
  (+ 1 (/ (rand-int 900) 100.0)))

(defn- set-price
  [com-m nick server price]
  (let [attrs (key-attrs nick server)]
    (if (= price 0)
      (destroy! :price attrs)
      (update! :price attrs (assoc attrs :price price)))))

(defn- stock-exists?
  [com-m nick server]
  (when-let [stock-map (fetch-one :price
                                  :where (key-attrs nick server))]
    stock-map))

;; FIXME: Should delisting a stock should return its current value to
;; any owners' accounts?
(defn- delist-stock
  [com-m nick server]
  (println "delisting: " (destroy! :shares {:stock nick :server server}))
  (set-price com-m nick server 0))

(defn- get-price
  [{:keys [com] :as com-m} nick server]
  (let [user-map (fetch-one :price
                            :where (key-attrs nick server))
        db-price (get user-map :price 0.0)
        _ (println "Got price " db-price " for " nick)
        price    (if (and (validate-stock nick server)
                          (= db-price 0.0))
                   (let [ipo-price (rand-price)]
                     (set-price com-m nick (:server @com) ipo-price)
                     (send-message com-m
                                   (str "New IPO: " nick " at "
                                        (fmt-currency ipo-price)))
                     ipo-price)
                   db-price)]
    price))

(defn- get-market
  [server]
  (let [price-maps (fetch :price
                          :where (keyed [server]))]
    (println "got" price-maps "for server" server)
    price-maps))

(defn- get-ownership
  [nick server stock]
  (let [stock (.toLowerCase stock)
        owner-maps (fetch :shares
                        :where (keyed [server stock]))]
    (println "got" owner-maps "for server" server "stock" stock)
    owner-maps))

(defn- get-stocks
  [nick server]
  (let [stock-maps (fetch :shares
                          :where (key-attrs nick server))]
    stock-maps))

(defn- get-shares
  [nick server stock]
  (let [stock-map (fetch-one :shares
                            :where (key-attrs nick server stock))]
    (get stock-map :shares 0)))

(def limit (ref {}))

(let [scheduler (Executors/newScheduledThreadPool 1)]
  (defn schedule [^Runnable task]
    (.schedule scheduler task
               5 TimeUnit/MINUTES)))

(defn- change-shares
  [stock new-shares {:keys [^String nick com bot] :as com-m}]
  (let [msg
        (cond
         (.equalsIgnoreCase nick stock)
         (str nick ": Buying shares in yourself is insider trading.")
         :else
         (do
           (set-shares nick (:server @com) stock new-shares)
           (str nick ": You now own " new-shares " shares of \u00a7" stock
                " at " (fmt-currency (get-price com-m stock (:server @com))) ".")))]
    (send-message com-m msg)))

(defn- fmt-share
  ;; FIXME: Fix calling convention
  [{:keys [stock shares] :as com-m} server & fmt-price?]
  (let [base (str shares " shares of \u00a7" stock)]
    (if fmt-price?
      (let [price (get-price com-m stock server)]
        (str base " at " (fmt-currency price)))
      base)))

(defn- fmt-owner
  [{:keys [nick shares]}]
  (str "\u00a7" nick " (" shares ")"))

(defn shares-fn
  "Create a plugin command function that applies f to the stock owned by the user specified in args."
  [f]
  (fn [{:keys [nick com args] :as com-m}]
    (let [stock (first args)
          shares (get-shares nick (:server @com) stock)
          new-shares (f shares)]
      (if (validate-stock stock (:server @com))
        (do
          (let [old-price   (get-price com-m stock (:server @com))
                price-delta (+ 0.01 (/ (rand-int 100) 100.0))]
            (cond
             (< new-shares shares) (set-price com-m stock (:server @com)
                                              (max 0.01 (- old-price price-delta)))
             (> new-shares shares) (set-price com-m stock (:server @com)
                                              (max 0.01 (+ old-price price-delta)))))
          (when (not= new-shares shares)
            (change-shares stock new-shares com-m)))
        (send-message
           com-m
           (str nick ": " stock " has not been seen, and cannot be bought or sold."))))))

(defn total-value
  [com-m stocks]
  (reduce + (map #(* (:shares %)
                     (get-price com-m (:stock %) (:server %))) stocks)))

(def print-portfolio
  (fn [{:keys [nick com bot args] :as com-m}]
    (let [server (:server @com)]
      (send-message
       com-m
       (let [stocks (get-stocks nick server)]
         (if (> (count stocks) 0)
           (str
            nick ": You own "
            (oxford-list (map #(fmt-share % server true) stocks))
            " (totalling "
            (fmt-currency (total-value com-m stocks)) ").")
           (str nick " does not own any shares.")))))))

(def print-price
  (fn [{:keys [nick com args] :as com-m}]
    (let [stock (first args)]
      (if stock
        (send-message
         com-m
         (if-let [price (get-price com-m stock (:server @com))]
           (str nick ": \u00a7" stock " is currently priced at "
                (fmt-currency price) ".")
           (str nick ": " stock " is not listed on the exchange.")))
        (send-message
         com-m
         (str nick ": No stock symbol specified."))))))


(def print-shares
  (fn [{:keys [nick com args] :as com-m}]
    (let [stock (first args)]
      (if stock
        (send-message com-m
                      (let [shares (get-shares nick (:server @com) stock)
                            price  (get-price com-m stock (:server @com))]
                        (if shares
                          (str nick ": You own " shares " shares of \u00a7" stock
                               " at " (fmt-currency price)
                               " (" (fmt-currency (* shares price)) " total).")
                          (str nick ": I have no record for shares of \u00a7" stock
                               " owned by " nick "."))))
        (print-portfolio com-m)))))


(def print-ownership
  (fn [{:keys [nick com bot args] :as com-m}]
    (let [stock (or (first args) nick)]
      (send-message
       com-m
       (let [owners (get-ownership nick (:server @com) stock)]
         (case (count owners)
           0 (str nick ": No users own shares in \u00a7" stock ".")
           1 (str nick ": The only user who owns shares in \u00a7" stock
                  " is " (fmt-owner (first owners)))
           (str nick ": The users who own shares in \u00a7" stock " are "
                (oxford-list (map fmt-owner (reverse (sort-by :shares owners))))
                ".")))))))

(def print-ticker
  (fn [{:keys [nick com bot args] :as com-m}]
    (let [prices (get-market (:server @com))]
      (println (str "number returned: " (count prices)))
      (println (map #(fmt-currency (:price %)) prices))
      (send-message
       com-m
       (str "Current market values: "
            (s/join "; " (map #(str "\u00a7" (:nick %) ":"
                                    (fmt-currency (:price %)))
                              prices)))))))

(def buy (shares-fn #(+ % 100)))
(def sell (shares-fn #(- % 100)))

(def margin-order
  (fn [{:keys [nick] :as  com-m}]
    (send-message com-m (str nick ": You don't have a margin account!"))))

(def delist
  (fn [{:keys [nick com args] :as com-m}]
    (when-privs
     com-m :admin
     (let [stock (or (first args) nick)]
       (delist-stock com-m stock (:server @com))
       (send-message
        com-m
        (str nick ": " stock " has been delisted from the exchange."))))))

(def validate
  (fn [{:keys [nick com args] :as com-m}]
      (let [stock (or (first args) nick)]
        (if (validate-stock stock (:server @com))
          (send-message
           com-m
           (str nick ": " stock " is a legal symbol."))
          (send-message
           com-m
           (str nick ": " stock " has not been seen, and cannot be bought or sold."))))))

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
   "Check whether a symbol is a nick that has been seen on this server."
   #{"validate"} validate)
  (:cmd
   "Issue a sell-short order. (Unimplemented)"
   #{"short"} margin-order)
  (:cmd
   "Shows the shares owned in the given stock."
   #{"ownership" "owners" "owner" "own"} print-ownership)
  (:indexes [[:server :channel :nick]]))
