(ns io.simplect.compose.util)

(defn var-arglist-and-doc
  [fvar]
  (select-keys (meta fvar) [:arglists :doc]))

(defn merge-meta
  [target-var m]
  (alter-meta! target-var #(merge % m)))

(defmacro fref
  [nm fname]
  `(let [m# (var-arglist-and-doc (var ~fname))]
     (def ~nm ~fname)
     (merge-meta (var ~nm) (update-in m# [:doc] #(str "Notation for [[" '~fname "]].\n\n" %)))
     '~nm))
