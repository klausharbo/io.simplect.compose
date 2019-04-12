;;  Copyright (c) Klaus Harbo. All rights reserved.
;;  The use and distribution terms for this software are covered by the
;;  Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;  which can be found in the file epl-v10.html at the root of this distribution.
;;  By using this software in any fashion, you are agreeing to be bound by
;;  the terms of this license.
;;  You must not remove this notice, or any other, from this software.

(ns io.simplect.compose.notation
  (:require
   [clojure.core]
   [io.simplect.compose]
   [io.simplect.compose.util			:as u]))

(defmacro Ξ
  "Abbreviated form of [[io.simplect.compose/curry]]."
  [& args]
  `(io.simplect.compose/curry ~@args))
(u/merge-meta #'Ξ (u/var-arglist-and-doc #'io.simplect.compose/curry))
(alter-meta! #'Ξ (fn [m] (update-in m [:doc] #(str "Abbreviated form of [[io.simplect.compose/curry]].\n\n" %))))

(defmacro λ
  [& args]
  `(fn ~@args))
(u/merge-meta #'λ (u/var-arglist-and-doc #'fn))
(alter-meta! #'λ (fn [m] (update-in m [:doc] #(str "Abbreviated form of [[clojure.core/fn]].\n\n" %))))

(u/fref Π clojure.core/partial)
(u/fref π io.simplect.compose/partial1)
(u/fref γ clojure.core/comp)
(u/fref Γ io.simplect.compose/rcomp)
(u/fref μ clojure.core/map)
(u/fref ρ clojure.core/reduce)
