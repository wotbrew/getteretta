(ns getteretta
  (:require [clojure.set :as set]))

(defmacro getter
  "Generate a first class getter"
  [type getter]
  (let [sym (gensym)]
    `(fn [~(with-meta sym {:tag type})]
       (~getter ~sym))))

(defmacro setter
  "Generate a first class setter"
  [type setter]
  (let [sym (gensym)]
    `(fn [~(with-meta sym {:tag type}) val#]
       (~setter ~sym val#))))

(defmacro mapify**
  [type mac & forms]
  `(hash-map
     ~@(->> (for [[key g] (partition 2 forms)]
              `[~key (~mac ~type ~g)])
            (apply concat))))

(defmacro getter-map
  "Create a map of functions wrapping java getters with the correct type hints
  e.g
  (getter-map MyClass
    :foo .getFoo
    :bar .getbar)}"
  [type & getters]
  `(mapify** ~type getter ~@getters))

(defmacro setter-map
  "Create a map of functions wrapping java setters with the correct type hints
  e.g
  (setter-map MyClass
    :foo .setFoo
    :bar .setBar)}"
  [type & setters]
  `(mapify** ~type setter ~@setters))
