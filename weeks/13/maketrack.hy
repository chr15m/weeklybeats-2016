#!/usr/bin/env hy

; TODO: 
; * shuffle pyramid fx
; * bridge groups build-up/break-down

(import os)
(import sys)
(import math)
(import random)
(import [pprint [pprint]])

(require hy.contrib.loop)

(import [autotracker [generate MIDDLE_C samples]])
(import [autotracker.it.sample [Sample_File Sample_FileSlice Sample_KS]])
(import [autotracker.keys [Key_Minor Key_Major]])
(import [autotracker.strategies [Strategy_Main]])
(import [autotracker.generators [Generator_Bass Generator_ProbabilityTable Generator_Callback Generator_AmbientMelody Generator_Breaks]])

(import [utils [print-through get-random-sample get-random-bleep ftom mtof get-wrapped value-or-callable dir-to-samples here]])

(import [sfxr [sfxr-genetics]])
(import [fx [apply-fx-to-pattern apply-drop-groups-to-pattern]])
(import [tables [beats random-squared]])

(def breakbeat-pattern [1 0 2 0  0 0 2 0])

(defn make-fractal-note-sequence [sequence-length number-of-notes &optional [basic-sequence-length 4] [sparseness-probability 0.75]]
 (let [[basic-sequence (random.sample (range number-of-notes) basic-sequence-length)]]
    (list-comp
      (if (> (random.random) sparseness-probability)
        nil
        (get basic-sequence (% (int (/ t (random.choice [1 2 4]))) (len basic-sequence))))
      [t (xrange sequence-length)])))

(defn make-hats-fn [hat-samples &optional [volume 64] [hat-probability 0.90] [hat-frequency 4] [trill-probability 0.0625]]
  (let [[pitch-map (dict (list-comp (, h (random.randint 60 72)) [h hat-samples]))]]
    (fn [channel-number pattern strategy rhythm beat-begin beats-length key-root key-chord]
      (for [row (xrange beat-begin (+ beat-begin beats-length))]
        (when (not (% row hat-frequency))
          (let [[sample (if (> (random.random) 0.9) (random.choice hat-samples) (get hat-samples 0))]
                [pitch (get pitch-map sample)]]
            (when (< (random.random) hat-probability)
              (setv (get (get pattern.data row) channel-number) 
                [pitch sample (value-or-callable volume row) 0 0])
              ; hat trill
              (when (< (random.random) trill-probability)
                (let [[trill-width (random.choice [1 1 2 2 3])]
                      [trill-length (random.choice [3 4 6 8])]]
                  (for [row-trill (xrange row (min (+ row trill-length) (len pattern.data)) trill-width)]
                    (setv (get (get pattern.data row-trill) channel-number) 
                      [pitch sample (value-or-callable volume row true) 0 0])))))))))))

(defn make-melody-fn [sample root-note sequence notes-set &optional [pace 4] [volume 64] [note-length None] [octave 0] [note-end-type :cut]]
  (fn [channel-number pattern strategy rhythm beat-begin beats-length key-root key-chord]
    ; (print strategy.pat_idx)
    (for [row (xrange beat-begin (+ beat-begin beats-length))]
      (let [[current-pace (value-or-callable pace strategy.pat_idx row)]
            [current-note-length (or (value-or-callable note-length) current-pace)]
            [current-octave (value-or-callable octave strategy.pat_idx row)]
            [note (get sequence (% (int (/ row current-pace)) (len sequence)))]
            [note-end (+ row current-note-length)]]
        (when (and (not (or (= note nil) (= (get notes-set note) nil)))
                ; if we're on the right row division for this pace
                (= (% row current-pace) 0))
          (setv (get (get pattern.data row) channel-number)
            [(+ (get notes-set note) (+ root-note (* 12 current-octave))) sample volume 0 0])
          ; note stop (if inside the pattern)
          (when (< note-end (len pattern.data))
            (setv (get (get pattern.data (+ row current-note-length)) channel-number)
              [(if (= note-end-type :cut) 254 255) 0 127 0 0])))))))

; TODO: break repeats
; TODO: break shift segments
; TODO: break substitutions
; TODO: more kinds of break rhythm
; TODO: match breaks with beats?
; TODO: techdiff-fx.txt
; TODO: fx.txt
(defn make-breaks-fn [sample-chunks-break sample-bassdrum sample-snaredrum &optional
                      [break-pitch 60] [break-pace 4] [beat-pace 4] [match-beat true] [seed random.random]]
  (let [[r (random.Random (value-or-callable seed))]
        [bass-snare-rhythm (r.choice [[1 0 2 0  0 1 2 0]
                                           [0 1 2 0  1 0 2 0]
                                           [1 0 0 1  0 0 2 0]
                                           [1 0 2 1  0 2 0 0]])]
        [break-rhythm (r.choice [[0 1 2 3  4 5 6 7  8 9 10 11  12 13 14 15]
                                      [0 1 2 0  1 2 3 4  8 9 10 8  9 10 13 15]])]]
    (fn [channel-number pattern strategy rhythm beat-begin beats-length key-root key-chord]
      (for [row (xrange beat-begin (+ beat-begin beats-length))]
        (let [[tick (/ row break-pace)]
              [break-match (% row break-pace)]]
          ; lay down the breakbeat
          (if (= break-match 0)
            (setv (get (get pattern.data row) channel-number)
              [break-pitch (get-wrapped sample-chunks-break (get-wrapped break-rhythm tick)) 64 0 0]))
          ; lay down the back-beat
          (if match-beat
            ; backbeat should match the break
            (if (= break-match 0)
              (let [[drum-type (get-wrapped breakbeat-pattern (get-wrapped break-rhythm tick))]]
                (when drum-type
                  (setv (get (get pattern.data row) (+ channel-number 1))
                    [60 (get [sample-bassdrum sample-snaredrum] (- drum-type 1)) 64 0 0]))))
            ; backbeat should be original
            (if (= (% row beat-pace) 0)
              (let [[which-drum (get-wrapped bass-snare-rhythm (/ row beat-pace))]]
                (if (> which-drum 0)
                  (setv (get (get pattern.data row) (+ channel-number 1))
                    [60 (get [sample-bassdrum sample-snaredrum] (- which-drum 1)) 64 0 0]))))))))))

(defn make-random-placement-fn [sample-set &optional [seed random.random] [volume 64] [probability 0.75] [number-per-pattern-segment 1]]
  (let [[r (random.Random seed)]
        [positions (list-comp (r.randint 0 16) [x (range 16)])]
        [chances (list-comp (r.random) [x (range 16)])]]
    (fn [channel-number pattern strategy rhythm beat-begin beats-length key-root key-chord]
      (for [n (range number-per-pattern-segment)]
        (when (< (get-wrapped chances (/ beat-begin beats-length)) probability)
          (let [[row (+ beat-begin (% (* (get-wrapped positions (/ beat-begin beats-length)) 4) beats-length))]
                [sample (get-wrapped sample-set (int (/ beat-begin beats-length)))]]
            (setv (get (get pattern.data row) channel-number)
              [60 sample volume 0 0])))))))

(defn make-probability-table-fn [sample &optional [seed random.random] [probability-table []] [pitch 60] [pace 4] [trippy false]]
  (let [[r (random.Random (value-or-callable seed))]]
    (fn [channel-number pattern strategy rhythm beat-begin beats-length key-root key-chord]
      (for [row (xrange beat-begin (+ beat-begin beats-length))]
        (when (and (< (r.random) (get-wrapped probability-table (/ row pace)))
                (or trippy (= (% row pace) 0)))
          (setv (get (get pattern.data row) channel-number) [pitch sample (if (= (% (/ row pace) 2) 0) 64 32) 0 0]))))))

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

(defn transform-notes-multiply [notes]
  (let [[multiplier (random.randint 0 12)]]
    (list-comp (% (* multiplier n) 12) [n notes])))

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

(defn main [argv]
  (generate
    argv
    (fn [itf]
      (setv itf.tempo (random.randint 175 185))
      (print itf.tempo "BPM")
      (let [[samples-bass-sounds (list-comp (itf.smp_add (Sample_File :name (% "sfxr-evolved-bass-%d" x) :filename (sfxr-genetics "./sfxr-bass/" (% "sfxr-evolved-bass-%d" x)))) [x (range 3)])]
            ; [sample-bassdrum (itf.smp_add (Sample_File :name "bassdrum" :filename (get-random-sample "CanOfBeats" "bd")))]
            ;[sample-bassdrum (itf.smp_add (Sample_File :name "bassdrum-evolved" :filename (sfxr-genetics "./sfxr-drums/bassdrum" "bassdrum")))]
            ; [sample-snaredrum (itf.smp_add (Sample_File :name "snaredrum" :filename (get-random-sample "CanOfBeats" "sd")))]
            ;[sample-snaredrum (itf.smp_add (Sample_File :name "snaredrum-evolved" :filename (sfxr-genetics "./sfxr-drums/snare" "snaredrum")))]
            [sample-hi-bleep (itf.smp_add (Sample_File :name "hi-bleep" :filename (get-random-bleep "hi") :loop "sustain"))]
            [sample-lo-bleep (itf.smp_add (Sample_File :name "lo-bleep" :filename (get-random-bleep "lo") :loop "sustain"))]
            [samples-drums (sum (list-comp [(itf.smp_add (Sample_File :name (% "bassdrum-evolved-%d" x) :filename (sfxr-genetics "./sfxr-drums/bassdrum" (% "bassdrum-%d" x))))
                                            (itf.smp_add (Sample_File :name (% "snaredrum-evolved-%d" x) :filename (sfxr-genetics "./sfxr-drums/snare" (% "snaredrum-%d" x))))] [x (range 8)]) [])]
            [sample-break (random.choice ["amen.wav" "think.wav"])]
            [break-chunk-count 8]
            [sample-chunks-break (list-comp (itf.smp_add (Sample_FileSlice :filename (os.path.join samples sample-break) :slices break-chunk-count :which s)) [s (range break-chunk-count)])]
            [length-break-chunk (len (getattr (get itf.smplist (- (get sample-chunks-break 0) 1)) "data"))]
            [length-beat (int (* 44100 (/ 60.0 itf.tempo)))]
            [break-note (ftom (* (mtof 60) (/ length-break-chunk length-beat) (/ break-chunk-count 4)))]
            [samples-weirdos (list-comp (itf.smp_add (Sample_File :name (+ "weird-" (first (.split s "."))) :filename (sfxr-genetics (+ "./sfxrs/" (first (.split s "."))) (+ "weird-" (first (.split s ".")))))) [s (os.listdir "sfxrs")])]
            ; compute a two basic sequences of notes using the fractal melody method
            [sequences (list-comp (make-fractal-note-sequence (random.choice [16 32]) 4 :sparseness-probability (+ (* (random.random) 0.5) 0.4)) [x (range 3)])]
            ; bass variation
            [sequence-bass (slice (list (reversed (get sequences 0))) 0 16)]
            ; select some notes randomly to map to the sequence
            [notes-set (get-good-notes 4)]
            [notes-sets [notes-set (transform-notes-flip notes-set) (transform-notes-multiply notes-set)]]
            [melody-sample (random.choice (+ [sample-hi-bleep] samples-bass-sounds))]
            [bass-sample (random.choice (+ [sample-lo-bleep] samples-bass-sounds))]

            [melody-fns-main (list-comp (make-melody-fn melody-sample 60 (get sequences x) (get notes-sets x) :pace 4 :volume 40) [x (range 3)])]
            [melody-fns-bass (list-comp (make-melody-fn bass-sample 48 (get [sequence-bass sequence-bass (get sequences 1)] x) (get notes-sets x) :pace 8 :volume 64) [x (range 3)])]
            [breaks-fns (list-comp (make-breaks-fn sample-chunks-break (get-wrapped samples-drums (* x 2)) (get-wrapped  samples-drums (+ (* x 2) 1))  :break-pitch (int (math.floor break-note)) :seed (random.random)) [x (range 3)])]
            [weirdos-fns (list-comp (make-random-placement-fn (slice samples-weirdos (* x 3) (+ (* x 3) 3)) :volume 48 :seed (random.random) :number-per-pattern-segment 2 :probability 0.9) [x (range 3)])]  
            [drop-groups (sorted [[1 5 6 7] [1 5 6] [1 4] [1 6] [0 1] [0 1 3] [0 5 6 7] [0 3 4]] :key (fn [x] (len x)))]
            [never-drop [2]]

            [master-key (if (< (random.random) 0.6) Key_Minor Key_Major)]
            [root (+ 12 (random.randint 50 (+ 50 12 -1)))]
            [strategy (Strategy_Main root master-key 128 32)]]

        (strategy.gen_add (Generator_Callback 1 (make-section-lookup-fn melody-fns-main [0 0 0 0 1 1 1 1 0 0 0 0 2 2 2 2])))
        (strategy.gen_add (Generator_Callback 1 (make-section-lookup-fn melody-fns-bass [0 0 0 0 1 1 1 1 0 0 0 0 2 2 2 2])))
        (strategy.gen_add (Generator_Callback 1 (make-section-lookup-fn weirdos-fns [0 0 0 0 1 1 1 1 2 2 2 2])))
        
        (strategy.gen_add (Generator_Callback 2 (make-section-lookup-fn breaks-fns [0 0 0 0 1 1 1 1 2 2 2 2 1 1 1 1])))
        
        (let [[beat-seed (random.random)]]
          (strategy.gen_add (Generator_Callback 1 (make-probability-table-fn (get samples-drums 6) :probability-table (-> beats (get :hiphop) (get :bd)) :seed beat-seed :trippy false)))
          (strategy.gen_add (Generator_Callback 1 (make-probability-table-fn (get samples-drums 7) :probability-table (-> beats (get :hiphop) (get :sd)) :seed beat-seed :trippy false))))
        
        (let [[samples-808-hihat (list-comp (itf.smp_add (Sample_File :name (+ "808-hihat-" (unicode b)) :filename (get-random-sample "808" "hi hat-snappy"))) [b (xrange 3)])]]
          (strategy.gen_add (Generator_Callback 1 (make-hats-fn samples-808-hihat))))

        (for [i (xrange 28)]
          (print "pattern" i)
          (itf.ord_add (itf.pat_add (strategy.get_pattern))))
        
        (print "Applying post-fx")
        (let [[seeds (list-comp (random.random) [x (range 4)])]
              [pattern [0 0 0 1 2 2 2 3]]
              [drop-group-base-seed (random.random)]]
          (for [p (range (len itf.patlist))]
            (print "pattern" p)
            (apply-fx-to-pattern (get itf.patlist p) 3 :seed (get seeds (get-wrapped pattern p)))
            ; double whammy
            (apply-fx-to-pattern (get itf.patlist p) 3)
            ; also do 808 snares
            (apply-fx-to-pattern (get itf.patlist p) 6)
            ; drop-groups make things less or more busy
            (apply-drop-groups-to-pattern (get itf.patlist p) p drop-groups never-drop :seed drop-group-base-seed)))
        itf))))

(if (= __name__ "__main__")
  (main sys.argv))
