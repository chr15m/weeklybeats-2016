(import os)
(import json)
(import random)
(import [glob [glob]])
(import [subprocess [Popen PIPE]])

(import [chipvolver [load_definitions reproduce]])

(defn sfxr-render [definition filename]
  (.communicate (Popen ["./jsfxr/sfxr-to-wav" filename] :stdout PIPE :stdin PIPE) (json.dumps definition)))

(defn sfxr-genetics [startswith name]
  (print "sfxr genetics:" startswith name)
  (let [[wav-file-name (+ "samples/" name "-evolved.wav")]
        [already-generated (os.path.isfile wav-file-name)]
        [[sample-evolved-definition seed-used] (reproduce (load_definitions (glob (+ startswith "*.sfxr.json"))) :seed (random.random))]]
    (if (not already-generated)
      (sfxr-render sample-evolved-definition wav-file-name))
    wav-file-name))

