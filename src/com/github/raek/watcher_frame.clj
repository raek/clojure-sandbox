(ns com.github.raek.watcher-frame
    (:use clojure.contrib.swing-utils)
    (:import (javax.swing JFrame JLabel)))

(def watchers (ref {}))

(defn update-label [key reference old-state new-state]
  (let [[frame label] (@watchers key)]
    (do-swing (.setText label (str @reference))
	      (.pack frame))))

(defn watch [reference]
  (let [label (JLabel. (str @reference))
	frame (doto (JFrame.) (.add label) .pack .show)
	key (gensym "watcher-frame")]
    (dosync (commute watchers assoc key [frame label]))
    (add-watch reference key update-label)))
