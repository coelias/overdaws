(ns overdaws.samples.drums
  (:use overtone.core)
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))


(def code->name
  {:SS "SnareSoft"
   :C "Crash"
   :H "Hat"
   :T "Tom"
   :RB "RideBell"
   :ST "SideStick"
   :SH "SnareHard"
   :SM "SnareMed"
   :SR "SnareRimshot"
   :R "Ride"
   :HC "Hat Closed"
   :HH "Hat Half Open"
   :HO "Hat OpenHard"
   :HM "Hat OpenMed"
   :HP "Hat Pedal"
   :HT "Hat Tight"
   :KH "Kick Hard"
   :KM "Kick Med"
   :KS "Kick Soft"})

(defn ->collection [d]
  (->> d
       (map butlast)
       (group-by first)
       (map (fn [[k v]]
              (let [code (keyword k)]
                [code { :name (code code->name)
                       :versions (apply max (map
                                             (comp #(Integer/parseInt %) second)
                                             v))}])))
       (into {})))

(def drum-collections
  (->> "drums/"
       io/file
       .list
       (filter #(.endsWith % ".wav"))
       (map #(string/split % #"\."))
       (map first)
       (map #(string/split % #"-"))
       (group-by last)
       (map (fn [[k v]] [(keyword k) (->collection v)]))
       (into {})))

(defn check-collection [c]
  (some? (get drum-collections c)))

(defn check-code [c]
  (->> drum-collections
       vals
       (mapcat keys)
       (into #{})
       (#(get % c))
       some?))

(defn find-param
  ([pred l]
   (first (filter pred l)))
  ([pred l default]
   (or
    (first (filter pred l))
    default)))

(defn collections
  ([]
   (let [k (keys drum-collections)]
     (clojure.pprint/pprint k)
     k))
  ([col]
   (let [c (col drum-collections)]
     (clojure.pprint/pprint c)
     c)))

(def load-drum-sample
  (memoize (fn [code n coll]
             (load-sample
              (format "drums/%s-%d-%s.wav" (name code) n (name coll))))))

(def drum
  (memoize (fn [& params]
     (if-let [code (find-param check-code params)]
       (let [coll (find-param check-collection params :CloseRoom)
             n (find-param int? params 1)]
         (load-drum-sample code n coll))))))

(defsynth play-sample [buf 0 amp 1.0 pan 0.0 rate 1.0]
  (let [;; Read the audio from the buffer
        audio (play-buf 2 buf :rate rate :action FREE)
        ;; Apply panning and volume control
        stereo-audio (pan2 audio pan amp)]
    (out 0 stereo-audio)))


;(let [dd (drum 4 :SH :Balcony)]
;  (play-sample dd)
;  )
