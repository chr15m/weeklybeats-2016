(import os)
(import math)
(import random)

(import [autotracker [samples]])

(def here (os.path.dirname __file__))

(defn get-random-bleep [t]
  (os.path.join samples (random.choice (list-comp f [f (os.listdir samples)] (and (f.startswith "c64") (in (+ "-" t ".wav") f))))))

(defn get-random-sample [subfolder starts-with]
  (os.path.join samples subfolder (random.choice (list-comp f [f (os.listdir (os.path.join samples subfolder))] (and (f.startswith starts-with) (f.endswith ".wav"))))))

(defn fx-code [c] (- (ord c) 96))

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

