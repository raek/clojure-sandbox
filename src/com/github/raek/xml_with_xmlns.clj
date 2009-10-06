;; by Rasmus Svensson, raek@lysator.liu.se, http://raek.se/
;; October 6, 2009

;; Copyright (c) Rasmus Svensson. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns #^{:author "Rasmus Svensson"
    :doc "Extension of clojure.xml providing XML namespaces support
    
    Example:
    
    (with-xmlns {:tag :foo:bar,
                 :attrs {:xmlns:foo \"http://example.com/ns\"
                         :foo:key \"value\"}
                 :content nil})
    => {:tag :http://example.com/ns/bar,
        :attrs {:http://example.com/ns/key \"value\"},
        :content []}
    
    Note that the reader prints keywords with a slash between the namespace and
    and the keyword name. This slash is just reader syntax and is not a part of
    the namespace itself. To extract the namespace and name parts of a keyword,
    use the core functions (namespace) and (name).
    
    (namespace :http://www.w3.org/XML/1998/namespace/id)
      => \"http://www.w3.org/XML/1998/namespace\"
    (name :http://www.w3.org/XML/1998/namespace/id)
      => \"id\""
    (:require clojure.xml))

(defn- ns-decl-prefix
  "Returns the namespace prefix of the namespace declaration attribute key.
  
  A namespace declaration attribute key on the form :xmlns:foo will
  yield :foo. A key that is not a part of a namespace declaration will yield
  nil.
  
  (ns-decl-prefix :xmlns:foo) => :foo
  (ns-decl-prefix :other) => nil"
  [attr-name]
  (nth (re-find #"^xmlns:(.+)$" attr-name) 1))

(defn- ns-binding
  "Treats an attribute as a namespace declaration and returns a pair
  representing its prefix-namspace binding.
  
  If the attribute is not a namespace declaration, this function will return
  nil.
  
  (ns-binding [:xmlns:foo \"http://example.com/ns\"])
    => [:foo \"http://example.com/ns\"]
  (ns-binding [:xmlns \"http://example.com/ns\"])
    => [nil \"http://example.com/ns\"]
  (ns-binging [:other \"value\"])
    => nil"
  [[attr-key attr-val]]
  (if (= attr-key :xmlns)
    [nil attr-val]
    (let [prefix (ns-decl-prefix (name attr-key))]
      (if (nil? prefix)
	nil
	[(keyword prefix) attr-val]))))

(defn- ns-decls
  "Makes a prefix-namespace map from the namespace delcarations in attrs."
  [attrs]
  (into {} (map ns-binding attrs)))

(defn- is-ns-decl?
  "Determines whether the given attribute is a namespace declaration."
  [attr]
  (let [[key val] attr]
    (boolean (re-find #"^xmlns(:(.+))?$" (name key)))))

(defn- split-qname
  "Splits a qualified name into a pair of its prefix and local name parts.
  
  A qualified name without a prefix yields a pair where the prefix part is nil.
  
  (split-qname :foo:bar) => [:foo :bar]
  (split-qname :baz) => [nil :baz]"
  [qname]
  (let [match (re-find #"(.*):(.*)" (name qname))]
    (if (nil? match)
      [nil qname]
      [(keyword (nth match 1)) (keyword (nth match 2))])))

(defn- name-with-ns
  "Expands a qualified name by looking up the prefix in the bindings.
  
  The returned keyword has its namespace set to the URI looked up in the
  namespace bindings.
  
  (name-with-ns :foo:bar {:foo \"http://example.com/ns\"})
    => :http://example.com/ns/bar"
  [qname ns-bindings]
  (let [[prefix local-part] (split-qname qname)]
    (keyword (ns-bindings prefix)
	     (name local-part))))

(defn- attrs-with-ns
  "Applies (name-with-ns) to all attribute names."
  [attrs ns-bindings]
  (into {} (map (fn [[key val]]
		    [(name-with-ns key ns-bindings) val])
		(remove is-ns-decl? attrs))))

(def with-xmlns)

(defn- content-with-ns
  "Applies (with-xmlns) to all the content nodes."
  [content ns-bindings]
  (vec (map (fn [node]
		(with-xmlns node ns-bindings))
	    content)))

(defn with-xmlns
  "Traverses an XML tree and adds their namespace onto every tag and attribute
  name.
  
  The tag and attribute names are keywords with their namespaces set to the
  corresponding namespace URI.
  
  This function also removes the namespace declaration attributes."
  ([node]
   (with-xmlns node {nil nil
	             :xml "http://www.w3.org/1999/xhtml"
		     :xmlns "http://www.w3.org/2000/xmlns/"}))
  ([node parent-ns-bindings]
   (if (string? node)
     node
     (let [{:keys [tag attrs content]} node
	   node-ns-bindings (merge parent-ns-bindings
				   (ns-decls attrs))]
       {:tag (name-with-ns tag node-ns-bindings)
        :attrs (attrs-with-ns attrs node-ns-bindings)
        :content (content-with-ns content node-ns-bindings)}))))

(defn parse-with-xmlns
  "As clojure.xml/parse, but applies (with-xmlns) on the returned tree."
  ([s]
   (with-xmlns (clojure.xml/parse s)))
  ([s startparse]
   (with-xmlns (clojure.xml/parse s startparse))))
