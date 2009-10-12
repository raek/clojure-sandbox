(ns user
    (:require [com.github.raek.xmlns :as xmlns]
	      [clojure.xml :as xml])
    (:use clojure.contrib.pprint))

(def x (xml/parse "xmlns-example.xml"))
(println "xml tree without namespace parsing:")
(pprint x)
(newline)

(def y (xmlns/with-xmlns x))
(println "xml tree with namespace parsing:")
(pprint y)
