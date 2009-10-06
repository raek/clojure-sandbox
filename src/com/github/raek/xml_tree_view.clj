(defn average [coll]
  (/ (reduce + coll) (count coll)))

(defn tree-levels [tree]
  (cond (nil? tree) 0
	(string? tree) 1
	:else (inc (reduce max 0 (map tree-levels (tree :content))))))

(defn tree-width [tree metrics]
  (cond (nil? tree) 0
	(string? tree) (string-width tree metrics)
	:else (max (reduce + (map #(tree-width % metrics) (tree :content)))
		   (string-width (name (tree :tag)) metrics))))

(defn string-width [string metrics]
  (+ (* 2 (.stringWidth metrics " "))
     (.stringWidth metrics string)))

(defn level-height [metrics]
  (.getHeight metrics))

(defn tree-height [tree metrics]
  (if (nil? tree)
    0
    (+ (level-height metrics)
       (* 2 (level-height metrics) (dec (tree-levels tree))))))

(import 
 '(java.awt Color Graphics Dimension)
 '(java.awt.image BufferedImage)
 '(javax.swing JPanel JFrame))