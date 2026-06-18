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

(def start [name bpm bar-len]
  (let [kname (keyword name)]
    (when (kname @daw-state)
      (throw (ex-info "Daw alread exists") {:name kname}))
    (swap! daw-state assoc :kname {:synths {:click (click)
                                            :click-accent (click-accent)}
                                   :bpm bpm
                                   :barlen bar-len
                                   :metro (metronome bpm)
                                   })
    )




  )


@daw-state
