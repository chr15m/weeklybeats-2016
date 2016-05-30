(def beats
  {:hiphop
   {:bd [1.00 0.10 0.90 0.10  0.05 0.10 0.01 0.10  0.80 0.10 0.70 0.10  0.05 0.01 0.01 0.20]
    :sd [0.00 0.05 0.05 0.05  0.95 0.00 0.00 0.20  0.00 0.05 0.00 0.00  0.95 0.00 0.00 0.10]
    :hh [0.95 0.05 0.95 0.05  0.95 0.10 0.95 0.10  0.95 0.05 0.95 0.05  0.95 0.05 0.95 0.05]}})

(defn random-squared [&optional {:length 16}]
  (list-comp (pow (random.random) 2) [p (xrange length)]))