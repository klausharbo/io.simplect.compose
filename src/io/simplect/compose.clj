;;  Copyright (c) Klaus Harbo. All rights reserved.
;;  The use and distribution terms for this software are covered by the
;;  Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;  which can be found in the file epl-v10.html at the root of this distribution.
;;  By using this software in any fashion, you are agreeing to be bound by
;;  the terms of this license.
;;  You must not remove this notice, or any other, from this software.

(ns io.simplect.compose
  (:require
   [cats.core					:as cats]
   [clojure.algo.generic.functor		:as functor]
   [clojure.spec.alpha				:as s]
   [clojure.spec.test.alpha			:as t]
   ,,
   [io.simplect.compose.util					:refer [fref]]
   ))

(defn- build-sym
  [nm]
  (symbol (str (ns-name *ns*)) (name nm)))

(defn- def-*
  [nm docstring form]
  `(do ~(if docstring
          `(def ~nm ~docstring ~form)
          `(def ~nm ~form))
       (alter-meta! (var ~nm) #(assoc % :private true))
       '~nm))

(defmacro def-
  "Like `clojure.core/def` except `nm` is private to namespace."
  ([nm form]
   (def-* nm nil form))
  ([nm docstring form]
   (def-* nm docstring form)))

(def ^:dynamic *instrument?* false)

(defmacro without-instrumentation
  [& body]
  `(binding [*instrument?* false]
     (do ~@body)))

(defmacro with-instrumentation
  [& body]
  `(binding [*instrument?* true]
     (do ~@body)))

(defn- fdef*
  [nm specpred docstring form]
  (let [sym (build-sym nm)
        M (meta nm)
        instr? (if (find M :instrument) (get M :instrument) *instrument?*)]
    `(do ~(if docstring
            `(def ~nm ~docstring ~form)
            `(def ~nm ~form))
         ~(when specpred
            `(s/fdef ~nm :args (s/cat ~(keyword (name (gensym))) ~specpred)))
         ~(when instr?
            `(t/instrument '~sym))
         '~sym)))

(defmacro fdef
  "Defines a function accepting a single value using `def` (instead of `defn`).  The resulting
  function is instrumented to be checked against `specpred` which must be a predicate function
  taking a single argument.

  `nm` will be instrumented iff (1) the form is embedded in `with-instrumentation` (without an
  intervening `without-instrumentation` form), (2) the dynamic var `*instrument?*` is truthy, or (3)
  its metadata contains a truthy value for key `:instrument`.

  Example:

        user> (fdef my-add-3 int? (fn [x] (+ x 3)))
        user/my-add-3
        user> (my-add-3 2)
        5
        user> (my-add-3 2.0)
        Execution error - invalid arguments to io.simplect.compose/my-add-3 at (REPL:5).
        2.0 - failed: int? at: [:G__8490]
        user>"
  ([nm specpred docstring form]
   (fdef* nm specpred docstring form))
  ([nm specpred-or-docstring form]
   (if (string? specpred-or-docstring)
     (fdef* nm nil specpred-or-docstring form)
     (fdef* nm specpred-or-docstring nil form)))
  [[nm form]
   (fdef* nm nil nil form)])

(defn- fdef-*
  [nm specpred docstring form]
  (let [sym (build-sym nm)]
    `(do
       ~(fdef* nm specpred docstring form)
       (alter-meta! (var ~sym) #(assoc % :private true))
       '~sym)))

(defmacro fdef-
  "Like `fdef` except defines a private function."
  ([nm specpred docstring form]
   (fdef-* nm specpred docstring form))
  ([nm specpred-or-docstring form]
   (if (string? specpred-or-docstring)
     (fdef-* nm nil specpred-or-docstring form)
     (fdef-* nm specpred-or-docstring nil form)))
  [[nm form]
   (fdef-* nm nil nil form)])

(defn- sdefn*
  [nm docstring arglist spec body]
  (let [sym (build-sym nm)
        M (meta nm)
        instr? (if (find M :instrument) (get M :instrument) *instrument?*)]
    `(do
       (defn ~nm
         ~@(when docstring [docstring])
         ~arglist
         ~@body)
       (s/fdef ~sym :args ~spec)
       ~(when instr?
          `(t/instrument '~sym))
       '~sym)))

(defmacro sdefn
  "Define instrumented function.  Like `clojure.core/defn` but instruments function to be checked
  against `spec`.

  `nm` will be instrumented iff (1) the form is embedded in `with-instrumentation` (without an
  intervening `without-instrumentation` form), (2) the dynamic var `*instrument?*` is truthy, or (3)
  its metadata contains a truthy value for key `:instrument`."
  [nm spec docstring arglist & body]
  (sdefn* nm docstring arglist spec body))

(defn- sdefn-*
  [nm docstring arglist spec body]
  (let [sym (build-sym nm)]
    `(do
       ~(sdefn* nm docstring arglist spec body)
       (alter-meta! (var ~sym) #(assoc % :private true))
       '~sym)))

(defmacro sdefn-
  "Liks `sdefn` except defines a private function."
  ([nm spec docstring arglist & body]
   (sdefn-* nm docstring arglist spec body)))

(defn >->>
  "Calls `f` with arguments reordered such that the first argument to `>->>` is given to `f` as the
  last.  The name of the function is meant to suggest that `f` is converted to fit into a '->'
  context by mapping argument order from `->`-style (arg first) to '->>'-style (arg last).

  Can be called without arguments in which case a function reordering arguments is
  returned (cf. `ex3` below).

  Example:

        user> (let [f (fn [& args] args)
                    map> (>->> map)]
                {:ex1 (>->> 1 f 2 3 4 5),
                 :ex2 (-> (range 5)
                          (>->> map (partial * 10)))
                 :ex3 (-> (range 5)
                          (map> (partial * 1000)))})
        {:ex1 (2 3 4 5 1),
         :ex2 (0 10 20 30 40),
         :ex3 (0 1000 2000 3000 4000)}
        user>"
  ([v & f-and-args]
   (let [f (first f-and-args), f-args (concat (rest f-and-args) [v])]
     (apply f f-args)))
  ([f]
   (fn [& args] (apply f (concat (rest args) [(first args)])))))

(defn >>->
  "Calls `f` with arguments reordered such that last argument to `>>->` is given to `f` as the
  first. The name of the function is meant to suggest that `f` is converted to fit into a '->>'
  context by mapping argument order from `->>`-style (arg last) to '->'-style (arg first).

  Can be called without arguments in which case a function reordering arguments is
  returned (cf. `ex3` below).

  Example:

        user> (let [f (fn [& args] args)
                    assoc>> (>>-> assoc)]
                {:ex1 (>>-> f 1 2 3 4 5),
                 :ex2 (->> {:a 1}
                           (>>-> assoc :b 2))
                 :ex3 (->> {:a 1}
                           (assoc>> :b 2))
                 :ex4 ((partial >>-> assoc :b 2) {:a 1})})
        {:ex1 (5 1 2 3 4),
         :ex2 {:a 1, :b 2},
         :ex3 {:a 1, :b 2}
         :ex4 {:a 1, :b 2}}
        user>"
  ([f & args]
   (let [f-args (list* (last args) (butlast args))]
     (apply f f-args)))
  ([f]
   (fn [& args] (apply f (concat (list (last args)) (butlast args))))))

(defn rcomp
  "Like `clojure.core/comp` except applies `fs` in reverse order."
  [& fs]
  (apply comp (reverse fs)))

(defn partial>
  "Like `partial` except it will insert the argument accepted by the returned function between first
  and second elements of `args` (as opposed to `partial` which adds the argument after those given
  to it).

  Example:

        user> {:ex1 ((partial> assoc :x 2) {:a 1})
               :ex2 (->> [{:a 1} {:v -1}]
                         (map (partial> assoc :x 2)))}
        {:ex1 {:a 1, :x 2},
         :ex2 ({:a 1, :x 2} {:v -1, :x 2})}
        user>"
  [& args]
  (apply partial >>-> args))

(sdefn reorder (s/cat :indices (s/coll-of (s/and int? (complement neg?)) :kind sequential?) :f fn?)
  "Returns a function which calls `f` with arguments reordered according to `v` which must be a
  sequential collection of integers all less than the number of arguments.

  Example:

        user> (let [f (fn [& args] args)
                    g (reorder [0 3 2 0] f)]
                (g :a :b :c :d :e))
        (:a :d :c :a)
        user>"
  [v f]
  (fn [& args]
    (let [mx (apply max v)]
      (if (< mx (count args))
        (apply f (map (partial nth args) v))
        (throw (ex-info (str "reorder: " mx " arg index too large ")
                 {:v v, :max-index mx, :args args}))))))

(defmacro curry
  "Same as `cats.core/curry` from the Funcool Cats library, its docstring reproduced below for
  convenience.

  Given either a fixed arity function or an arity and a function, return another which is curried.

  With inferred arity (function must have one fixed arity):

      (defn add2 [x y] (+ x y))
      (def cadd2 (curry add2))

      ((cadd2 1) 3)
      ;; => 4

      (cadd2 1 3)
      ;; => 4

  With given arity:

      (def c+ (curry 3 +))

      ((c+ 1 2) 3)
      ;; => 6

      ((((c+) 1) 2) 3)
      ;; => 6"
  [& args]
  `(cats/curry ~@args))

;; Add fmap from clojure.algo.generic.functor
(fref fmap functor/fmap)
