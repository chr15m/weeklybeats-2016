#!/usr/bin/env hy

(import os)
(import sys)
(import math)
(import random)

(require hy.contrib.loop)

(import [autotracker [generate MIDDLE_C samples]])
(import [autotracker.it.sample [Sample_File Sample_FileSlice Sample_KS]])
(import [autotracker.keys [Key_Minor Key_Major]])
(import [autotracker.strategies [Strategy_Main]])
(import [autotracker.generators [Generator_Bass Generator_ProbabilityTable Generator_Callback Generator_AmbientMelody Generator_Breaks]])

(def here (os.path.dirname __file__))

(defn get-random-bleep [t]
  (os.path.join samples (random.choice (list-comp f [f (os.listdir samples)] (and (f.startswith "c64") (in (+ "-" t ".wav") f))))))

(defn get-random-sample [subfolder n]
  (os.path.join samples subfolder (random.choice (list-comp f [f (os.listdir (os.path.join samples subfolder))] (and (f.startswith n) (f.endswith ".wav"))))))

(defn fx [c] (- (ord c) 96))

(defn get-wrapped [array index]
  (get array (% (int index) (len array))))

(defn null-pattern-fn [&rest args])

(defn value-or-callable [v &rest args] (if (callable v) (apply v args) v))

; taken from pure-data
(defn mtof [m]
  (cond
    [(<= m -1500) 0]
    [(> m 1499) (mtof 1499)]
    [true (* (math.exp (* m .0577622650)) 8.17579891564)]))

; taken from pure-data
(defn ftom [f]
  (if (> f 0)
    (* (math.log (* f .12231220585)) 17.3123405046)
    -1500))

(defn dir-to-samples [d itf] (list-comp
                               (itf.smp_add (Sample_File :name (os.path.basename f) :filename (os.path.join d f)))
                               [f (os.listdir d)]
                               (f.endswith ".wav")))

; from CanOfBeats
(def bd-prob [1 0.1 0.9 0.1  0.05 0.1 0.01 0.1  0.8 0.1 0.7 0.1  0.05 0.01 0.01 0.2])
(def sd-prob [0 0.05 0.05 0.05  0.95 0 0 0.2 0 0.05 0 0  0.95 0 0 0.1])
(def hh-prob [0.95 0.05 0.95 0.05  0.95 0.1 0.95 0.1   0.95 0.05 0.95 0.05  0.95 0.05 0.95 0.05])

(defn totally-random-prob [] (list-comp (pow (random.random) 2) [p (xrange 16)]))

(defn hat-probability [tick]
  (get [0.95 0.05 0.1 0.05] (% tick 4)))

(defn set-pattern-value! [pattern channel-number row value]
  (setv (get (get pattern.data row) channel-number) value))

; {"hello" 22 "boo" 59}
;(def weighted-random [weights]
;  (random.choice (sum (list-comp k [k (range )]) [])))

; TODO: convert make fns to this
(defn add-generator [strategy channel-count generator-fn]
  (strategy.gen_add
    (Generator_Callback
      channel-count
      (fn [channel-number pattern strategy rhythm beat-begin beats-length key-root key-chord]
        (for [row (xrange beat-begin (+ beat-begin beats-length))]
          (generator-fn
            strategy.pat_idx
            channel-number
            channel-count
            row
            (fn [channel-number row value]
              (set-pattern-value! pattern channel-number row value))))))))

(defn make-fractal-note-sequence [sequence-length number-of-notes &optional [basic-sequence-length 4] [sparseness-probability 0.75]]
 (let [[basic-sequence (random.sample (range number-of-notes) basic-sequence-length)]]
    (list-comp
      (if (> (random.random) sparseness-probability)
        nil
        (get basic-sequence (% (int (/ t (random.choice [1 2 4]))) (len basic-sequence))))
      [t (xrange sequence-length)])))

; TODO: compute trill mask and use it
(defn make-hats-fn [hat-samples]
  (let [[trill-mask (list-comp x [x (range 16)])]])
  (fn [channel-number pattern strategy rhythm beat-begin beats-length key-root key-chord]
    (for [row (xrange beat-begin (+ beat-begin beats-length))]
      (let [[prob (hat-probability row)]
            [sample (if (> (random.random) 0.9) (random.choice hat-samples) (get hat-samples 0))]
            [pitch 60]
            [volume (get [255 127 255 127] (% row 4))]]
        (when (< (random.random) prob)
          (setv (get (get pattern.data row) channel-number) 
            [pitch sample volume 0 0]))))))

; TODO: specify note length
(defn make-melody-fn [sample root-note sequence notes-set &optional [pace 4] [volume 255] [octave 0]]
  (fn [channel-number pattern strategy rhythm beat-begin beats-length key-root key-chord]
    ; (print strategy.pat_idx)
    (for [row (xrange beat-begin (+ beat-begin beats-length))]
      (let [[current-pace (value-or-callable pace strategy.pat_idx row)]
            [current-octave (value-or-callable octave strategy.pat_idx row)]
            [note (get sequence (% (int (/ row current-pace)) (len sequence)))]]
        (when (and (not (or (= note nil) (= (get notes-set note) nil)))
                ; if we're on the right row division for this pace
                (= (% row current-pace) 0))
          (setv (get (get pattern.data row) channel-number)
            [(+ (get notes-set note) (+ root-note (* 12 current-octave))) sample volume 0 0]))))))

; TODO: break repeats
; TODO: break shift segments
; TODO: break substitutions
; TODO: more kinds of break rhythm
; TODO: match breaks with beats?
; TODO: techdiff-fx.txt
(defn make-breaks-fn [sample-chunks-break sample-bassdrum sample-snaredrum &optional [break-pitch 60] [break-pace 4] [beat-pace 4]]
  (let [[bass-snare-rhythm (random.choice [[1 0 2 0  0 1 2 0]
                                           [0 1 2 0  1 0 2 0]
                                           [1 0 0 1  0 0 2 0]
                                           [1 0 2 1  0 2 0 0]])]
        [break-rhythm (random.choice [[0 1 2 3  4 5 6 7  8 9 10 11  12 13 14 15]
                                      [0 1 2 0  1 2 3 4  8 9 10 8  9 10 14 15]])]]
    (fn [channel-number pattern strategy rhythm beat-begin beats-length key-root key-chord]
      (for [row (xrange beat-begin (+ beat-begin beats-length))]
        ; lay down the breakbeat
        (if (= (% row break-pace) 0)
        (setv (get (get pattern.data row) channel-number)
          [break-pitch (get-wrapped sample-chunks-break (get-wrapped break-rhythm (/ row break-pace))) 255 0 0]))
        ; lay down the back-beat
        (if (= (% row beat-pace) 0)
          (let [[which-drum (get-wrapped bass-snare-rhythm (/ row beat-pace))]]
            (if (> which-drum 0)
              (setv (get (get pattern.data row) (+ channel-number 1))
                [60 (get [sample-bassdrum sample-snaredrum] (- which-drum 1)) 255 0 0]))))))))

; eyeballed
(def note-jump-probabilities [5 5 5 5 5 7 7 7 3 3 3 6 6 2 2 4 4 1])

(defn get-good-notes [n]
  ; (random.sample (range 0 12) (+ (max sequence) 1))
  (loop [[c (dec n)] [notes [(random.randint 0 12)]]]
    (if (> c 0)
      (recur
        (dec c)
        (+ notes [(% (+ (get notes -1) (random.choice note-jump-probabilities)) 12)]))
      (sorted notes))))

(defn transform-notes-flip [notes]
  (let [[pivot (random.randint 0 12)]]
    (list-comp (% (+ (* (- n pivot) -1) pivot) 12) [n notes])))

(defn make-section-lookup-fn [section-fns sections-pattern]
  (fn [channel-number pattern strategy &rest args]
    (apply
      (get section-fns (get-wrapped sections-pattern strategy.pat_idx))
      (+ [channel-number pattern strategy] (list args)))))

(defn make-octave-noodler-fn []
  ; always return the same result for a particular lookup
  (let [[seed (random.random)]]
    (fn [pattern row]
      (let [[r (random.Random)]]
        (r.seed (.format "octave-noodler-{0}-{1}" seed (int (/ row 8))))
        (random.choice [0 0 0 0 1 -1 -2])))))

(defn make-pace-noodler-fn []
  (let [[noodles (sum (list-comp ((fn [p] (* [p] (min p 4))) (random.choice [1 2 8])) [x (range 1024)]) [])]]
    (fn [pattern row]
      (get-wrapped noodles (+ (* pattern 128) row)))))

; TODO: procedural vocals

(generate
  sys.argv
  (fn [itf]
    (setv itf.tempo (random.randint 175 185))
    (print itf.tempo "BPM")
    (let [[sample-hi-bleep (itf.smp_add (Sample_File :name "hi-bleep" :filename (get-random-bleep "hi") :loop "sustain"))]
          [sample-lo-bleep (itf.smp_add (Sample_File :name "lo-bleep" :filename (get-random-bleep "lo") :loop "sustain"))]
          [sample-bass (itf.smp_add (Sample_KS :name "bass" :freq (/ MIDDLE_C 4) :decay 0.005 :nfrqmul 0.5 :filt0 0.2 :filtn 0.2 :filtf 0.005 :length_sec 0.7))]
          [sample-bassdrum (itf.smp_add (Sample_File :name "bassdrum" :filename (get-random-sample "CanOfBeats" "bd")))]
          [sample-snaredrum (itf.smp_add (Sample_File :name "snaredrum" :filename (get-random-sample "CanOfBeats" "sd")))]
          [sample-break (random.choice ["amen.wav" "think.wav"])]
          [break-chunk-count 8]
          [sample-chunks-break (list-comp (itf.smp_add (Sample_FileSlice :filename (os.path.join samples sample-break) :slices break-chunk-count :which s)) [s (range break-chunk-count)])]
          [length-break-chunk (len (getattr (get itf.smplist (- (get sample-chunks-break 0) 1)) "data"))]
          [length-beat (int (* 44100 (/ 60.0 itf.tempo)))]
          [break-note (ftom (* (mtof 60) (/ length-break-chunk length-beat) (/ break-chunk-count 4)))]
          ; [samples-sfxrs (dir-to-samples (os.path.join here "samples") itf)]
          ; compute a two basic sequences of notes using the fractal melody method
          [sequences (list-comp (make-fractal-note-sequence (random.choice [16 32]) 4 :sparseness-probability (+ (* (random.random) 0.5) 0.25)) [x (range 2)])]
          ; bass variation
          [sequence-bass (slice (list (reversed (get sequences 0))) 0 16)]
          ; select some notes randomly to map to the sequence
          [notes-set (get-good-notes 4)]
          [notes-sets [notes-set (transform-notes-flip notes-set)]]
          
          [melody-fns-main (list-comp (make-melody-fn sample-hi-bleep 60 (get sequences x) (get notes-sets x) 4 :volume 230) [x (range 2)])]
          [melody-fns-bass (list-comp (make-melody-fn sample-lo-bleep 60 sequence-bass (get notes-sets x) 8) [x (range 2)])]
          [melody-fns-noodler (list-comp (make-melody-fn sample-hi-bleep 72 (get sequences x) (get notes-sets x) :octave (make-octave-noodler-fn) :pace (make-pace-noodler-fn) :volume 230) [x (range 2)])]
          [master-key (if (< (random.random) 0.6) Key_Minor Key_Major)]
          [root (+ 12 (random.randint 50 (+ 50 12 -1)))]
          [strategy (Strategy_Main root master-key 128 32)]]
      
      (strategy.gen_add (Generator_Callback 2 (make-breaks-fn sample-chunks-break sample-bassdrum sample-snaredrum :break-pitch (int (math.floor break-note)))))
      (strategy.gen_add (Generator_Callback 1 (make-section-lookup-fn melody-fns-main [0 0 0 0 1 1 1 1 0 0 1 1])))
      (strategy.gen_add (Generator_Callback 1 (make-section-lookup-fn melody-fns-bass [0 0 0 0 1 1 1 1 0 0 1 1])))

      (strategy.gen_add (Generator_Callback 1 (make-section-lookup-fn melody-fns-noodler [0 0 0 0 1 1 1 1 0 0 1 1])))

      ;(let [[sample-808-bassdrum (itf.smp_add (Sample_File :name "808-bassdrum" :filename (get-random-sample "808" "bass")))]
      ;      [sample-808-snaredrum (itf.smp_add (Sample_File :name "808-snare" :filename (get-random-sample "808" "snare")))]
      ;      ; [samples-808-hihat (list-comp (itf.smp_add (Sample_File :name (+ "808-hihat-" (unicode b)) :filename (get-random-sample "808" "hi hat-snappy"))) [b (xrange 3)])]
      ;      ]
      ;  (strategy.gen_add (Generator_ProbabilityTable sample-808-bassdrum :probability-table bd-prob))
      ;  (strategy.gen_add (Generator_ProbabilityTable sample-808-snaredrum :probability-table sd-prob))
      ;  ;(strategy.gen_add (Generator_Callback 1 (make-hats-fn samples-808-hihat)))
      ;  )
      
      ;(for [s samples-sfxrs]
      ;  (strategy.gen_add (Generator_ProbabilityTable s :probability-table (random.choice [(totally-random-prob) bd-prob sd-prob]))))
      
      (for [i (xrange 24)]
        (print "pattern" i)
        (itf.ord_add (itf.pat_add (strategy.get_pattern))))
      itf)))

