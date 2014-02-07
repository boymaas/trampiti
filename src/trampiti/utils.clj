(ns trampiti.utils
  (:use [clojure.walk :only [prewalk]])
  (:require [clojure.string :as str])
  (:import [java.net URI URLEncoder]))

(defn url-encode     [s]     (URLEncoder/encode (str s) "utf8"))

;;; {:a {:b 1 :c [1 2 3]}} => {"a[b]" 1, "a[c]" [1 2 3]}
(defn- nested-param [params]            ; code copyed from clj-http
  (prewalk (fn [d]
             (if (and (vector? d) (map? (second d)))
               (let [[fk m] d]
                 (reduce (fn [m [sk v]]
                           (assoc m (str (name fk) \[ (name sk) \]) v))
                         {} m))
               d))
           params))

(defn query-string
  "Returns URL-encoded query string for given params map."
  [m]
  (let [m (nested-param m)
        param (fn [k v]  (str (url-encode (name k)) "=" (url-encode v)))
        join  (fn [strs] (str/join "&" strs))]
    (join (for [[k v] m] (if (sequential? v)
                           (join (map (partial param k) (or (seq v) [""])))
                           (param k v))))))
