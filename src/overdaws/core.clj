(ns app.core
  (:use overtone.live))

 (defn drm
  ""
  [ini beats & args]
  (let [timeblock (/ beats (count args) )]
    (loop [[f & r] args beg ini res '()]
      (if f
        (recur r (+ beg timeblock)
               (if (coll? f)
                 (concat res (apply drm beg  timeblock f))
                 (cons [f beg] res)))
        res))))

(defn sym->inst [k]
  (if (coll? k)
      (mapv sym->inst k)
      (if-let [[_ inst times] (re-find #"(.*)\*([0-9]+)" (name k))]
        (into [] (repeat (Integer/parseInt times) (keyword inst)))
        (keyword k))))

(defmacro drt [& args]
  (apply list drm 0 4 (map sym->inst args)))




(def kick (load-sample "drums/KickA-Hard-001-Balcony.wav"))




(defsynth play-sample [buf 0 amp 1.0 pan 0.0 rate 1.0]
  (let [;; Read the audio from the buffer
        audio (play-buf 2 buf :rate rate :action FREE)
        ;; Apply panning and volume control
        stereo-audio (pan2 audio pan amp)]
    (out 0 stereo-audio)))


;(def tempo (metronome 120)) ;; Set a metronome to 120 BPM
;
;(defn play-groove [beat-num]
;;; Schedule the kick on the beat
;  (prn beat-num)
;(at (tempo beat-num) (play-sample kick))
;
;;; Schedule the snare on the off-beat
;(at (tempo (+ 0.5 beat-num)) (play-sample kick))
;
;;; Recursively schedule the next beat
;(apply-at (tempo (inc beat-num)) play-groove (inc beat-num) []))
;
;;; Kick off the loop
;(play-groove (tempo))
;
;(clear-msg-queue)
;
;(tempo :bpm 24)
;
;(defsynth live-delay [in-bus 2 out-bus 0 delay-time 0.4 feedback 0.5]
;  (let [sig (sound-in in-bus)
;        wet (comb-c sig 2.0 delay-time feedback)]
;    (out out-bus (+ sig wet))))
;;
;;;; Start it
;(def my-delay (live-delay))
;;
;;(kill my-delay)
;;
;(play-sample kick :amp 1.0 :pan 0 :rate 1)
;
;(stop)
;
;
;(defsynth guitar-thru []
;  (out 0 (pan2 (sound-in 0) 0)))
;
;(def mon (guitar-thru))
;(kill mon)
;
;(defsynth guitar-wah [rate 2.0 depth 1000 center 1500 q 3.0 amp 1.0]
;         (let [sig  (sound-in 0)
;               lfo  (sin-osc:kr rate)
;               freq (+ center (* depth lfo))
;               wah  (b-peak-eq sig freq q 18)]
;           (out 0 (pan2 (* amp wah) 0))))
;
;(def wah (guitar-wah))
;
;
;
;
;(defsynth mouse-wah [q 3.0 amp 1.0]
;  (let [sig  (sound-in 0)
;        freq (mouse-x 400 3000 1)  ;; 1 = exponential, 0 = linear
;        wah  (b-peak-eq sig freq q 18)]
;    (out 0 (pan2 (* amp wah) 0))))
;
;(def wah (mouse-wah))
;
;(kill wah)



;;
;;(server-status)
;;
;(kill-server)

;(boot-server )
;; MIDIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
(-> (midi-connected-devices)
    count
    )
                                        ;(on-event [:midi :note-on]
                                        ;          (fn [msg] (println "PAD:" msg))
                                        ;          ::emp16-on)
                                        ;
                                        ;(on-event [:midi :control-change]
                                        ;          (fn [msg] (println "CC:" msg))
                                        ;          ::emp16-cc)

;;
;;(defsynth click []
;;  (let [env (env-gen (perc 0.001 0.05) :action FREE)
;;        sig (* env (sin-osc 1000))]
;;    (out 0 (pan2 sig 0))))
;;
;;;; Start the metronome at 120 BPM
;;(def metro (metronome 120))
;;
;;;; Schedule clicks on every beat
;;(defn play-metro [m beat]
;;  (at (m beat) (click))
;;  (apply-at (m (inc beat)) play-metro [m (inc beat)]))
;;
;;(play-metro metro (metro))
;;
;;(defn foo
;;  "I don't do a whole lot."
;;  [x]
;;  (println x "Hello, World!"))
