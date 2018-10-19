(ns equipage-test-assignment.core
  (:require [clojure.data.json :as json]
            [clj-time.core :as t]
            [clj-time.format :as f]))

(defmacro l [expr]
  `(do (println ~(pr-str expr) ":" ~expr)
       ~expr))

(defn module [x]
  (if (< x 0)
    (- x)
    x))

(defn delta-distance [[x1 y1] [x2 y2]]
  (Math/sqrt (+ (Math/pow (module (- x1 x2)) 2)
                (Math/pow (module (- y1 y2)) 2))))

(defn delta-time [t1 t2]
  (-> (t/in-seconds (t/interval t1 t2))
      (/ 60)
      (/ 60)))

;; Taken coordinates of 1km distance, so we could make sence of distance
;; roughly, +-10%
(def sample-point1 [27.634563446044922,
                    53.83824690756613])
(def sample-point2 [27.64812469482422,
                    53.838044323696])
(def distance-per-km (delta-distance sample-point1 sample-point2))

(defn distance->km [distance]
  (/ distance distance-per-km))

;; I assume they're sorted by date
(def points (json/read-str (json/read-str (slurp "resources/gistfile1.txt"))
                           :key-fn keyword
                           :value-fn (fn [key value]
                                       (if (= :at key)
                                         (f/parse value)
                                         value))))

;; km/h
(def speed-sanity-limit 240)
;; km/h / s
(def acceleration-sanity-limit 30)

(defn normalize
  "Filters obviously not legit points"
  [points]
  (->> (reduce (fn [acc {:keys [at position] :as point}]
                 (let [{past-at :at past-position :position past-speed :speed :as past-point} (last acc)
                       distance (delta-distance past-position position)
                       km (distance->km distance)
                       h (delta-time past-at at)
                       speed (l (/ km h))
                       acceleration (when past-speed (/ (- speed past-speed) (* h 60 60)))]
                   (cond (> speed speed-sanity-limit)
                         (do (println "FAILED speed sanitycheck") acc)

                         (and acceleration (> acceleration acceleration-sanity-limit))
                         (do (println "FAILED acceleration sanitycheck") acc)

                         :looks-legit
                         (conj acc (merge point
                                          {:speed speed})) ;; we need it for acceleration calc
                         )))
               [(first points)]
               (rest points))
       (map #(select-keys % [:at :position]))))

(comment (normalize points))
