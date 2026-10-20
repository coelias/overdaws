(ns overdaws.daw
  (:require [overdaws.schema :refer [->overdaws-state Daw]]
            [malli.core :as m])
  (:use overtone.core))

(def *daw* :default)
(defmacro with-daw [dawname & body]
  `(binding [*daw* ~dawname]
     ~@body))

(def state (->overdaws-state))

(defsynth click [amp 0.5]
  (let [env (env-gen (perc 0.001 0.05) :action FREE)
        sig (* amp env (sin-osc 1000))]
    (out 0 (pan2 sig 0))))

(defsynth click-accent [amp 0.8]
  (let [env (env-gen (perc 0.001 0.05) :action FREE)
        sig (* amp env (sin-osc 1500))]
    (out 0 (pan2 sig 0))))

(defn dval [& args]
  (get-in @state (cons *daw* args)))

(defn dval! [& args]
  (let [path (butlast args)
        val (last args)]
    (swap! state assoc-in (cons *daw* path) val)))

(defn metro-vol! [vol]
  (dval! :metro-vol vol))

(defn bpm! [val]
   (dval! :bpm val)
   ((dval :metro) :bpm val))

(defn barlen! [val]
   (dval! :barlen val))

(defn metro! []
  (if (zero? (dval :metro-vol))
    (do (metro-vol! 1) "Metronome ON")
    (do (metro-vol! 0) "Metronome OFF")))

(defn play-loop
  ([daw-name]
   (play-loop daw-name ((dval :metro))))
  ([daw-name beat]
   (let [m (dval :metro)
         bl (dval :barlen)
         mvol #(dval :metro-vol)]

     ;; Metronome player
     (let [sound (if (zero? (mod beat bl))
                   click-accent
                   click)]
       (apply-at (m (inc beat)) #(sound :amp (mvol)) []))
     (apply-at (m (inc beat)) #'play-loop [daw-name (inc beat)])
     ;;;;;;;;;;;;;;;;

     )))


(defn add-daw!
  ([bpm]
   (add-daw! :default bpm 4))
  ([bpm barlen]
   (add-daw! :default bpm barlen))
  ([daw-name bpm barlen]
   (if-let [x (daw-name @state)]
     "DAW ALREADY EXISTS"
     (do
       (swap! state assoc daw-name
              {
               :name daw-name
               :tracks {}
               :bpm bpm
               :barlen barlen
               :metro (metronome bpm)
               :metro-vol 0
               })
       (play-loop daw-name)))))

;(defn drums
;  ([& {:as drspecs}]
;   (apply drums :default drspecs))
;  ([dawname & {:as drspecs}]
;
;   ))


(-> @state)

(add-daw! 80)
(bpm! 80)
(metro!)
;
;(metro-vol! )
;
; (add-daw!)
; (metro!)
 (stop)
; (play-loop) 
