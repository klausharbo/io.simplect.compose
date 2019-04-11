(ns io.simplect.compose-test
  (:require
   [clojure.spec.alpha			:as s]
   [clojure.test				:refer [is deftest]]
   ,,
   [io.simplect.compose			:as c]))

(c/def- my-def 99)

(deftest def-internal
  (= true (-> #'my-def meta :private)))

(c/fdef myfn-int int? (fn [v] (inc v)))
(c/fdef myfn-int2 (s/cat :int int? :int int?) (fn [a b] (+ a b)))

(deftest fdef-test
  (is (= 10 (try (myfn-int 9) (catch Exception _ :error))))
  (is (= :error (try (myfn-int 9.0) (catch Exception _ :error))))
  (is (= 13 (try (myfn-int2 6 7) (catch Exception _ :error))))
  (is (= :error (try (myfn-int2 6.0 7) (catch Exception _ :error))))
  (is (= :error (try (myfn-int2 6 7.0) (catch Exception _ :error))))
  (is (= nil (-> #'myfn-int meta :private)))
  (is (= nil (-> #'myfn-int2 meta :private))))

(c/fdef- myfn-internal-int int? (fn [v] (inc v)))
(c/fdef- myfn-internal-int2 (s/cat :int int? :int int?) (fn [a b] (+ a b)))

(deftest fdef-internal-test
  (is (= 10 (try (myfn-internal-int 9) (catch Exception _ :error))))
  (is (= :error (try (myfn-internal-int 9.0) (catch Exception _ :error))))
  (is (= 13 (try (myfn-internal-int2 6 7) (catch Exception _ :error))))
  (is (= :error (try (myfn-internal-int2 6.0 7) (catch Exception _ :error))))
  (is (= :error (try (myfn-internal-int2 6 7.0) (catch Exception _ :error))))
  (is (= true (-> #'myfn-internal-int meta :private)))
  (is (= true (-> #'myfn-internal-int2 meta :private))))

(c/sdefn ^:instrument add2-with
  (s/cat :int int?)
  [v]
  (+ v 2))

(c/sdefn add2-without
  (s/cat :int int?)
  [v]
  (+ v 2))

(let [try-it (fn [f v]
               (try (f v)
                    (catch Exception e :error)))]
  (deftest instrumentation
    (is (= (try-it add2-with 4) 6))
    (is (= (try-it add2-without 4) 6))
    (is (= (try-it add2-with 4.0) :error))
    (is (= (try-it add2-without 4.0) 6.0))))

(deftest last-to-first
  (is (= [10 20 30 40] (-> (range 5)
                           (c/>->> drop 1)
                           (c/>->> mapv (partial * 10))))))

(deftest first-to-last
  (is (= {:a 1, :b 3} (->> {:a 1}
                           (c/>>-> assoc :b 3)))))


(let [fc (comp (partial * 10) (partial + 5))
      fr (c/rcomp (partial * 10) (partial + 5))]
  (deftest rcomp-test
    (is (= 100 (fc 5)))<
    (is (= 55 (fr 5)))))

(deftest partial-test
  (let [f (c/partial> assoc :a 3)]
    (is (= {:a 3, :b 9} (f {:b 9})))))

(deftest reorder-test
  (let [f (fn [& args] args)
        g (c/reorder [0 3 2 0] f)]
    (is (= [:a :d :c :a] (vec (g :a :b :c :d :e))))))

(let [f (c/curry 3 +)]
  (deftest curry-test
    (is (= 6 (f 1 2 3)))
    (is (= 6 ((f 1) 2 3)))
    (is (= 6 (((f 1) 2) 3)))))

(defn curryfn
  [a b c]
  (+ a b c))

(let [f (c/curry curryfn)]
  (deftest curry-test2
    (is (= 6 (f 1 2 3)))
    (is (= 6 ((f 1) 2 3)))
    (is (= 6 (((f 1) 2) 3)))))
