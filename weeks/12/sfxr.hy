(import json)
(import random)
(import [glob [glob]])
(import [subprocess [Popen PIPE]])

(import [chipvolver [load_definitions reproduce]])

(defn sfxr-render [definition filename]
  (.communicate (Popen ["./jsfxr/sfxr-to-wav" filename] :stdout PIPE :stdin PIPE) (json.dumps definition)))

(defn sfxr-genetics [startswith name]
  (let [[[sample-evolved-definition seed-used] (reproduce (load_definitions (glob (+ startswith "*.sfxr.json"))) :seed (random.random))]
        [wav-file-name (+ "samples/" name "-evolved.wav")]
        [sfxr-call (sfxr-render sample-evolved-definition wav-file-name)]]
    wav-file-name))

