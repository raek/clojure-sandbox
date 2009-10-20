(def vowels {:front "äeiöü", :back "aêîou"})

(def consonants "mpbvntdzjkgwsrl")

;; Borrowed from rhickey
;; (not yet--but planned to be--used)
(defn wrand 
  "given a vector of slice sizes, returns the index of a slice given a
  random spin of a roulette wheel with compartments proportional to
  slices."
  [slices]
  (let [total (reduce + slices)
        r (rand total)]
    (loop [i 0 sum 0]
      (if (< r (+ (slices i) sum))
        i
        (recur (inc i) (+ (slices i) sum))))))

(defn random-of [coll]
     (nth coll (rand-int (count coll))))

(defn random-vowel [backness]
  (random-of (get vowels backness)))

(defn random-consonant []
  (random-of consonants))

(defn random-syllable [backness]
  (let [type (rand-int 3)]
    (cond ;(= type 0) (list (random-vowel backness))
	  (= type 0) (list (random-consonant) (random-vowel backness))
	  (= type 1) (list (random-consonant) (random-vowel backness) (random-consonant))
	  (= type 2) (let [c2 (random-consonant)]
		       (list (random-consonant) (random-vowel backness) c2 c2)))))

(defn random-syllables [backness]
  (if (= 1 (rand-int 2))
    nil
    (concat (random-syllable backness) (random-syllables backness))))

(defn random-word []
  (let [backness (random-of [:front :back])]
    (apply str (concat (random-syllable backness) (random-syllables backness)))))

(defn random-words [count]
  (apply str (interpose \space (take count (repeatedly random-word)))))
