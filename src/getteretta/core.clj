(ns getteretta)

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

(defn run-setters
  "for each key/value pair, looks up the setter in the setter-map provided
   and executes it, passing the source instance, and the value.
   e.g. (run-setters obj {:foo .setFoo} {:foo \"bar\") =>
         obj with foo set to bar"
  ([source setter-map opts]
   (doseq [[key val] opts
           :let [setter (setter-map key)]
           :when setter]
     (setter source val))
   source)
  ([source setter-map key val & opts]
   (run-setters source setter-map (conj (partition 2 opts) [key val]))))

(defn setter-ctor
  "Takes a constructor function and a setter-map
   e.g. (def create-some-obj (setter-ctor #(SomeObj.) {:foo .setFoo}))
        (create-some-obj :foo 1)
        =>  an instance of SomeObj with the foo property set to 1."
  [ctor setter-map]
  (fn [& opts] (apply run-setters (ctor) setter-map opts)))