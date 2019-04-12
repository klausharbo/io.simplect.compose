(ns io.simplect.compose-test
  (:require
   [clojure.spec.alpha			:as s]
   [clojure.test				:refer [is deftest]]
   ,,
   [io.simplect.compose			:as c	:refer [>->> >>->]]))

(c/def- my-def 99)

(deftest test-def-
  (= true (-> #'my-def meta :private)))

(c/fdef myfn-int
  int?
  (fn [v]
    (inc v)))

(c/fdef- myfn-int2
  (s/cat :int int? :int int?)
  (fn [a b]
    (+ a b)))

(deftest test-fdef
  (is (= 10	(try (myfn-int 9) (catch Exception _ :err))))
  (is (= :err	(try (myfn-int 9.0) (catch Exception _ :err))))
  (is (= 13	(try (myfn-int2 6 7) (catch Exception _ :err))))
  (is (= :err	(try (myfn-int2 6.0 7) (catch Exception _ :err))))
  (is (= :err	(try (myfn-int2 6 7.0) (catch Exception _ :err))))
  (is (= nil	(-> #'myfn-int meta :private)))
  (is (= nil	(-> #'myfn-int meta :private))))

(c/fdef- myfn-internal-int int? (fn [v] (inc v)))
(c/fdef- myfn-internal-int2 (s/cat :int int? :int int?) (fn [a b] (+ a b)))

(deftest test-fdef-
  (is (= 10 (try (myfn-internal-int 9) (catch Exception _ :error))))
  (is (= :error (try (myfn-internal-int 9.0) (catch Exception _ :error))))
  (is (= 13 (try (myfn-internal-int2 6 7) (catch Exception _ :error))))
  (is (= :error (try (myfn-internal-int2 6.0 7) (catch Exception _ :error))))
  (is (= :error (try (myfn-internal-int2 6 7.0) (catch Exception _ :error))))
  (is (= true (-> #'myfn-internal-int meta :private)))
  (is (= true (-> #'myfn-internal-int2 meta :private))))

(c/sdefn add-to-int
  (s/cat :int int?)
  [v]
  (+ v 2))

(c/sdefn add-to-int2
  int?
  [v]
  (+ v 2))

(deftest test-sdefn
  (is (= (try (add-to-int 4) (catch Exception _ :error)) 6))
  (is (= (try (add-to-int 4.0) (catch Exception _ :error)) :error))
  (is (not (-> #'add-to-int meta :private)))
  (is (= (try (add-to-int2 4) (catch Exception _ :error)) 6))
  (is (= (try (add-to-int2 4.0) (catch Exception _ :error)) :error)))

(c/sdefn- add-to-int-private
  (s/cat :int int?)
  [v]
  (+ v 2))

(c/sdefn- add-to-int-private2
  int?
  [v]
  (+ v 2))

(deftest test-sdefn
  (is (= (try (add-to-int-private 4) (catch Exception _ :error)) 6))
  (is (= (try (add-to-int-private 4.0) (catch Exception _ :error)) :error))
  (is (-> #'add-to-int-private meta :private))
  (is (= (try (add-to-int-private2 4) (catch Exception _ :error)) 6))
  (is (= (try (add-to-int-private2 4.0) (catch Exception _ :error)) :error))
  (is (-> #'add-to-int-private2 meta :private)))

(deftest test->->>
  (is (= [10 20 30 40] (-> (range)
                           (>->> take 5)
                           (>->> drop 1)
                           (>->> mapv (partial * 10))))))

(deftest test->>->
  (is (= {:a 1, :b 3} (->> {:a 1}
                           (>>-> assoc :b 3)))))


(let [fc (comp (partial * 10) (partial + 5))
      fr (c/rcomp (partial * 10) (partial + 5))]
  (deftest test-rcomp
    (is (= 100	(fc 5)))
    (is (= 55	(fr 5)))))

(deftest test->partial
  (let [f (c/>partial assoc :a 3)]
    (is (= {:a 3, :b 9} (f {:b 9})))))

(deftest test-reorder
  (let [f (fn [& args] args)
        g (c/reorder [0 3 2 0] f)]
    (is (= [:a :d :c :a] (vec (g :a :b :c :d :e))))))

(defn curryfn
  [a b c]
  (+ a b c))

(let [f (c/curry 3 +)
      g (c/curry curryfn)]
  (deftest test-curry
    (is (= 6 (f 1 2 3)))
    (is (= 6 ((f 1) 2 3)))
    (is (= 6 (((f 1) 2) 3)))
    (is (= 6 (g 1 2 3)))
    (is (= 6 ((g 1) 2 3)))
    (is (= 6 (((g 1) 2) 3)))))


