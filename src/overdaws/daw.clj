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

(defn set-daw-val [name path val]
  (swap! daw-state assoc-in (cons name path) val))

(defn daw-getter [name]
  (fn [& args] (apply get-daw-val name args)))

(defn daw-setter [name]
  (fn [path val] (set-daw-val name path val)))

(def dval (daw-getter :default))
(def dval! (daw-setter :default))

(defn metro-vol!
  ([vol]
   (metro-vol! :default vol))
  ([name vol]
   (set-daw-val name [:metro-vol] vol)))

(defn bpm!
  ([val]
   (bpm! :default val)
   )
  ([name val]
   (set-daw-val name [:bpm] val)
   ((get-daw-val name :metro) :bpm val)))

(defn barlen!
  ([val]
   (barlen! :default val)
   )
  ([name val]
   (set-daw-val name [:barlen] val)))

(metro-vol! 1)

(get-daw-val :default :metro-vol)



(defn play-loop
  ([]
   (play-loop :default))
  ([dname]
   (play-loop dname ((get-daw-val dname :metro))))
  ([dname beat]
   (let [m (get-daw-val dname :metro)
         bl (get-daw-val dname :barlen)
         mvol #(get-daw-val dname :metro-vol)]

     ;; Metronome player
     (apply-at (m beat) #(click-accent :amp (mvol)) [])
     (doseq [x (range 1 bl)]
       (apply-at (m (+ x beat)) #(click :amp (mvol)) []))
    ;;;;;;;;;;;;;;;;

     (apply-at (m (+ beat bl)) #'play-loop [dname (+ bl beat)])))
  )



(defn start
  ([bpm bar-len]
   (start "default" bpm bar-len))
  ([name bpm bar-len]
   (let [kname (keyword name)
         met (metronome bpm)]
     (when (kname @daw-state)
       (throw (ex-info "Daw already exists" {:name kname})))
     (swap! daw-state assoc kname {:synths {}
                                   :bpm bpm
                                   :barlen bar-len
                                   :metro met
                                   :metro-vol 0})
     (play-loop kname))))

;(play-loop)

;(start 120 4)
(not 1)

(metro-vol! 1)
(barlen! 4)
(bpm! 120)

((dval :metro) :bpm 120)

(dval! [:barlen] 3)
