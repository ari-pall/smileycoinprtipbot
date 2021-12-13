(ns smileycoinprtipbot.core
  (:gen-class)
  (:require [tentacles.issues :as ti]
            [tentacles.search :as ts]
            [clojure.java.shell :only [sh] :as sh]
            ))

(defn find-if [f coll]
  (first (filter f coll)))

(defn update-timestamp-merges [new-timestamp]
  (spit "timestamp-merges" new-timestamp))

(defn update-timestamp-mentions [new-timestamp]
  (spit "timestamp-mentions" new-timestamp))

(defn get-timestamp-merges []
  (let [thing ((sh/sh "cat" "timestamp-merges") :out)]
    (if (= thing "")
      "2021-11-11T21:59:29Z"
      thing)))

(defn get-timestamp-mentions []
  (let [thing ((sh/sh "cat" "timestamp-mentions") :out)]
    (if (= thing "")
      "2021-11-11T21:59:29Z"
      thing)))

(defn timestamp->number [timestamp]
  (let [numchars #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9}]
    (read-string (apply str (filter #(contains? numchars %) timestamp)))))

(defn updatetime [pr] (pr :updated_at))

(defn updatetimenumber [pr] (timestamp->number (updatetime pr)))

(defn mergetime [pr] (get-in pr [:pull_request :merged_at]))

(defn mergetimenumber [pr] (timestamp->number (mergetime pr)))

(defn respond-to-merges [{:keys [user token pr-reward-amount repo repo-name repo-owner]}]
  (let [new-merges  (sort-by mergetimenumber
                             ((ts/search-issues "" {:repo repo
                                                    :is "pr"
                                                    :merged (str ">" (get-timestamp-merges))}
                                                {:oauth-token token})
                              :items))]
    (doseq [pr new-merges]
      (let [pr-er (get-in pr [:user :login])
            prn (pr :number)]
        (ti/create-comment repo-owner repo-name prn
                           (str "@" pr-er " Your pull request has been accepted. You can claim a Smileycoin reward by making a comment like this: `@" user " <your Smileycoin address>`")
                           {:oauth-token token})
        (update-timestamp-merges (mergetime pr))))))

(defn getbalance [] (read-string ((sh/sh "smileycoin-cli" "getbalance") :out)))

(def txfee 0.921)

(defn respond-to-mentions [{:keys [user token pr-reward-amount repo repo-name repo-owner]}]
  (let [new-mentions (sort-by updatetimenumber
                              ((ts/search-issues "" {:repo repo
                                                     :is "merged"
                                                     :updated (str ">" (get-timestamp-mentions))
                                                     :mentions user}
                                                {:oauth-token token})
                               :items))]
    (loop [remaining-mentions new-mentions]
      (if (and (not-empty remaining-mentions)
               (> (getbalance) (+ txfee pr-reward-amount)))
        (let [pr (first remaining-mentions)
              pr-er (get-in pr [:user :login])
              prn (pr :number)
              comments (ti/issue-comments repo-owner repo-name prn {:oauth-token token})
              bot-payment-comment (find-if #(and (= user (get-in % [:user :login]))
                                                 (clojure.string/includes? (% :body) "Smileycoins have been sent to"))
                                           comments)
              last-mentioning-comment (find-if #(and (= pr-er (get-in % [:user :login]))
                                                     (clojure.string/includes? (% :body) (str "@" user)))
                                               (reverse comments))]
          (do (update-timestamp-mentions (updatetime pr))
              (if (and last-mentioning-comment (not bot-payment-comment))
                (let [smlyaddress (str (first (last (re-seq #"([a-z]|[A-Z]|[0-9])+" (last-mentioning-comment :body)))))]
                  (sh/sh "smileycoin-cli" "sendtoaddress" smlyaddress (str pr-reward-amount))
                  (ti/create-comment repo-owner repo-name prn
                                     (str "@" pr-er " " pr-reward-amount " Smileycoins have been sent to " smlyaddress)
                                     {:oauth-token token})))
              (recur (rest remaining-mentions))))))))

(defn -main []
  (let [config (read-string ((sh/sh "cat" "config.edn") :out))
        strings (clojure.string/split (config :repo) #"/")
        len (count strings)
        config (assoc config :repo-owner (strings (- len 2))
                             :repo-name (strings (- len 1)))]
    (respond-to-mentions config)
    (respond-to-merges config)))




;; (sh/sh "smileycoin-cli" "listtransactions")

;; (def config (read-string ((sh/sh "cat" "config.edn") :out)))
;; (def user (config :user))
;; (def token (config :token))
;; (let [strings (clojure.string/split (config :repo) #"/")
;;       len (count strings)]
;;   (def repo-owner (strings (- len 2)))
;;   (def repo-name (strings (- len 1)))
;;   )
;; (def pr-reward-amount (config :pr-reward-amount))

;; (map #(% :body) (ti/issue-comments repo-owner repo-name 4 {:oauth-token token}))

;; ((sh/sh "smileycoin-cli" "help") :out)

;; sendtoaddress newaddress pr-reward-amount

;; (first (last (re-seq #"([a-z]|[A-Z]|[0-9])+" "@smlybot\n<sadfsas342f3>")))
;; (str (first (last (re-seq #"([a-z]|[A-Z]|[0-9])+" "<<<<<<<<<"))))
;; (re-find #"Smileycoins have been sent to" "Smileycoins have been sent    ")

;; (range 10000)

;; The options map only ever contains authentication info and/or optional input.
