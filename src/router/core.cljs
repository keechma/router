(ns router.core
  (:require [clojure.walk :refer [postwalk]]
            [clojure.set :refer [superset? union]]
            [router.util :refer [decode-query-params encode-query-params]]
            [clojure.string :as str]))

(def ^:private encode js/encodeURIComponent)

(defn ^:private process-url-namespace [v]
  (str/replace-first v "$" "/"))

(defn ^:private preserve-ns-url-key [k]
  (if (keyword? k)
    (let [k-ns (namespace k)
          k-name (name k)]
      (if k-ns (str k-ns "$" k-name) (name k)))
    k))

(defn ^:private keywordize-url-keys
  [m]
  (let [f (fn [[k v]] 
            (if (string? k) 
              [(keyword (process-url-namespace k)) v] 
              [k v]))]
    ;; only apply to maps
    (postwalk (fn [x] (if (map? x) (into {} (map f x)) x)) m)))

(defn ^:private preserve-ns-url-keys
  [m]
  (let [f (fn [[k v]] 
            (if (keyword? k)
              [(preserve-ns-url-key k) v]
              [k v]))]
    ;; only apply to maps
    (postwalk (fn [x] (if (map? x) (into {} (map f x)) x)) m)))


(defn ^:private placeholder->key [p]
  (-> (subs p 1)
      process-url-namespace
      keyword))

(defn ^:private process-route-part [default-keys part]
  (let [is-placeholder (= ":" (first part))
        key (when is-placeholder (placeholder->key part))
        has-default (contains? default-keys key)
        min-matches (if has-default "*" "+")
        re-match (if is-placeholder (str "(" "[^/]" min-matches ")") part)]
    {:is-placeholder is-placeholder
     :key key
     :has-default has-default
     :re-match re-match}))

(defn ^:private route-regex [parts]
  (let [base-regex (str/join "/" (map (fn [p] (:re-match p)) parts))
        full-regex (str "^" base-regex "$")]
    (re-pattern full-regex)))

(defn ^:private route-placeholders [parts]
  (->> parts
       (map :key)
       (remove nil?)))

(defn ^:private add-default-params [route]
  (if (vector? route) route [route {}]))

(defn ^:private strip-slashes
  ([route]
   (str/replace (str/trim (str route)) #"^/+|/+$" ""))
  ([side route]
   (case side
     :left (str/replace (str/trim (or route "")) #"^/+" "")
     :right (str/replace (str/trim (or route "")) #"/+$" "")
     route)))

(defn ^:private process-route [[route defaults]]
  (if (= :* route)
    {:parts []
     :regex #".*"
     :placeholders #{}
     :route route
     :defaults (or defaults {})
     :type ::catch-all}
    (let [parts (str/split route #"/")
          processed-parts (map (partial process-route-part (set (keys defaults))) parts)
          placeholders  (set (route-placeholders processed-parts))]
      {:parts processed-parts 
       :regex (route-regex processed-parts)
       :placeholders placeholders
       :route route
       :defaults (or defaults {})
       :type (if (empty? placeholders) ::exact ::pattern)})))

(defn ^:private remove-empty-matches [matches]
  (->> matches
      (filter (fn [[k v]]
                (and (not (nil? v))
                     (not (nil? k))
                     (not (empty? v))
                     (not= "null" v))))
      (into {})))

(defn ^:private expand-route [route]
  (let [strip-slashes (fn [[route defaults]] [(if (string? route) (strip-slashes route) route) defaults])]
    (-> route
        add-default-params
        strip-slashes
        process-route)))

(defn ^:private potential-route? [data {:keys [placeholders defaults] :as route}]
  (or (and (not (empty? placeholders))
           (superset? (set (keys data)) placeholders))
      (and (not (empty? defaults))
           (superset? (set defaults) (set data)))))

(defn ^:private intersect-maps [map1 map2]
  (reduce-kv (fn [m k v]
               (if (= (get map2 k) v)
                 (assoc m k v)
                 m)) {} map1))

(defn ^:private extract-query-param [default-keys placeholders m k v]
  (if-not (or (contains? default-keys k) (contains? placeholders k))
    (assoc m k v) 
    m))

(defn ^:private add-url-segment [defaults data url k]
  (let [val (get data k)
        placeholder (str ":" (preserve-ns-url-key k))
        is-default (= (get defaults k) val)
        replacement (if is-default "" (encode val))]
    (str/replace url placeholder replacement)))

(defn ^:private build-url [route data]
  (let [defaults (:defaults route)
        default-keys (set (keys defaults))
        placeholders (:placeholders route)
        query-params (reduce-kv (partial extract-query-param default-keys placeholders) {} data)
        base-url (reduce (partial add-url-segment defaults data) (:route route) placeholders)]
    (if (empty? query-params)
      (if (= "/" base-url) "" base-url)
      (str base-url "?" (encode-query-params (preserve-ns-url-keys query-params))))))

(defn ^:private route-score [data {:keys [defaults placeholders]}] 
  (reduce-kv 
   (fn [score k v]
     (cond
       (= v (get defaults k)) (+ score 1.1)
       (contains? placeholders k) (inc score)
       :else score)) 
   0 data))

(defn ^:private match-path-with-route [route url]
  (let [matches (first (re-seq (:regex route) url))]
    (when-not (nil? matches)
      (zipmap (:placeholders route) (rest matches)))))

(defn ^:private match-path [expanded-routes path] 
  (reduce 
   (fn [result route]
     (when-let [matches (match-path-with-route route path)]
       (reduced {:route (:route route)
                 :data  (merge (:defaults route) (remove-empty-matches matches))}))) 
   nil 
   expanded-routes))

;; Public API

(defn url->map
  "Accepts `expanded-routes` vector (returned by the `expand-routes` function)
  and a string as arguments. Returns a map which contains the data represented
  by the route.

  ```clojure
  ;; define routes
  (def routes [[\":page\", {:page \"index\"}]
                \":page/:id\"
                \":page/:id/:action\"]) 

  (def expanded-routes (expand-routes routes))

  (url->map expanded-routes \"foo\")
  ;; {:page \"foo\"}

  (url->map expanded-routes \"foo/1\")
  ;; {:page \"foo\" :id 1}

  (url->map expanded-routes \"foo?bar=baz\")
  ;; {:page \"foo\" :bar \"baz\"}
  ```
  "
  [expanded-routes url]
  (let [[u q] (str/split url #"\?")
        path (if (= u "/") u (strip-slashes :left u)) 
        query (remove-empty-matches (keywordize-url-keys (decode-query-params q)))
        matched-path (match-path expanded-routes path)]
    (if matched-path
      (assoc matched-path :data (merge query (:data matched-path)))
      {:data query})))

(defn map->url 
  "Accepts `expanded-routes` vector (returned by the `expand-routes` function)
  and a map as arguments. Returns a URL part which is the closest representatation
  of the data contained in the map (based on the `expanded-routes` argument).

  ```clojure
  ;; define routes
  (def routes [[\":page\", {:page \"index\"}]
                \":page/:id\"
                \":page/:id/:action\"]) 

  (def expanded-routes (expand-routes routes))

  (map->url expanded-routes {:page \"foo\"})
  ;; \"foo\"

  (map->url expanded-routes {:page \"foo\" :id 1})
  ;; \"foo/1\"

  (map->url expanded-routes {:page \"foo\" :id 1 :action \"bar\" :qux \"baz\"})
  ;; \"foo/1/bar?qux=baz\"
  ```
  "
  [expanded-routes data]
  (let [potential-routes (filter (partial potential-route? data) expanded-routes)]
    (if (empty? potential-routes)
      (str "?" (encode-query-params (preserve-ns-url-keys data)))
      (let [sorted-routes (sort-by (fn [r] (- (route-score data r))) potential-routes)
            best-match (first sorted-routes)]
        (build-url best-match data)))))

(defn expand-routes
  "Accepts a vector of routes as the argument. Returnes the expanded version
  of routes that can be passed to `url->map` and `map->url` functions.

  Elements in the route vector must be string (pattern) or vectors that contain
  the string pattern and default values for that route.

  ```clojure
  (def route \":page\")
  ;; This route will not be matched by an empty string

  (def route-with-defaults [\":page\", {:page \"index\"}])
  ;; This route will match an empty string and the :page key will hold 
  ;; the value \"index\"

  (expand-routes [[\":page\" {:page \"index\"}]
                  \":page/:action\"])
  ;; \"\" will be matched as {:page \"index\"}
  ;; \"foo/bar\" will be matched as {:page \"foo\" :action \"bar\"}
  ```
  "
  [routes]
  (let [expanded-routes (group-by :type (map expand-route routes))
   

        without-placeholders (filter #(not (seq (:placeholders %))) expanded-routes)
        with-placeholders (filter #(seq (:placeholders %)) expanded-routes)]
    ;; We put routes without placeholders at the start of the list, so they would
    ;; be matched first - exact matches have precedence over matches with placeholders
    ;;
    ;; Routes that have placeholders are sorted so that the routes with the most 
    ;; placeholders come first, because these have more specificity
    ;;
    ;; At the end of the list we put the catch-all route (if any exist)
    (vec (concat (expanded-routes ::exact)
                 (sort-by #(- (count (:placeholders %))) (expanded-routes ::pattern))
                 (expanded-routes ::catch-all)))))
