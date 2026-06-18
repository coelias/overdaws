(ns app.daw
  (:use overtone.core))

(def daw-state (atom {}))

(defsynth click [amp 0.5]
  (let [env (env-gen (perc 0.001 0.05) :action FREE)
        sig (* amp env (sin-osc 1000))]
    (out 0 (pan2 sig 0))))

(defsynth click-accent [amp 0.8]
  (let [env (env-gen (perc 0.001 0.05) :action FREE)
        sig (* amp env (sin-osc 1500))]
    (out 0 (pan2 sig 0))))

(defn get-daw-val [name & args]
  (get-in @daw-state (cons name args)))

(defn set-daw-val [name & args]
  ;; TODOOO
  )

(assoc-in)

(defn daw-getter [name]
  (fn [& args] (apply get-daw-val name args)))

(defn daw-setter [name]
  (fn [& args] (apply get-daw-val name args)))

(def gval (daw-getter :default))
(def pval (daw-setter :default))




(defn start
  ([bpm bar-len]
   (start "default" bpm bar-len))
  ([name bpm bar-len]
   (let [kname (keyword name)]
     (when (kname @daw-state)
       (throw (ex-info "Daw already exists" {:name kname})))
     (swap! daw-state assoc kname {:synths {:click (click)
                                            :click-accent (click-accent)}
                                   :bpm bpm
                                   :barlen bar-len
                                   :metro (metronome bpm)
                                   :metro-vol 0
                                   }))))

(def a (click :amp 1.0))
(a)

(start "asd2" 120 4)

(get-val :default :bpm)


@daw-state
