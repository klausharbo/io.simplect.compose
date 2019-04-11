;;  Copyright (c) Klaus Harbo. All rights reserved.
;;  The use and distribution terms for this software are covered by the
;;  Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;  which can be found in the file epl-v10.html at the root of this distribution.
;;  By using this software in any fashion, you are agreeing to be bound by
;;  the terms of this license.
;;  You must not remove this notice, or any other, from this software.

(ns io.simplect.compose.notation
  (:require
   [clojure.core				:as core]
   [io.simplect.compose				:as compose]
   [io.simplect.compose.util			:as u]))

(defmacro Ξ
  [& args]
  `(compose/curry ~@args))
(u/merge-meta #'Ξ (u/var-arglist-and-doc #'compose/curry))

(defmacro λ
  [& args]
  `(fn ~@args))
(u/merge-meta #'λ (u/var-arglist-and-doc #'fn))

(u/fref Π core/partial)
(u/fref π compose/partial>)
(u/fref γ core/comp)
(u/fref Γ compose/rcomp)
(u/fref μ core/map)
(u/fref ρ core/reduce)
