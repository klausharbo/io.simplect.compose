# Introduction

`compose` provides functions which make it more convenient to compose (sic!) functions in Clojure.

Clojure's 'threading macros' `->` and `->>` make it easy weave expressions together without creating
deeply nested expressions, allowing 

```
user> (let [v 50]
        (/ (+ (* v 2) 3) 4))
103/4
```

to become

```
user> (-> 50
          (* 2)
          (+ 3)
          (/ 4))
103/4
```

which permits you to build more complex expressions while maintaining reasonable clarity of what is
going on.

However, there are disadvantages to using this style too much: Big expressions are not very flexible
because they leave the functionality of your program nested inside expressions where they cannot be
reused.

The above very simple example consists of pieces of functionality: Multiplying by 2, Adding 3 and
dividing by 4.  Each are expressed clearly and directly, using the expressions `(* 2)`, `(+ 3)` and
`(/ 4)`, but not very flexibly: They cannot be easily **reused** nor can they be **tested**
separately, both quite significant disadvantages.

In many cases it is better to express functionalties directly, as functions. The definitions

```
(defn mult2 [v] (* v 2))
(defn add3 [v] (+ v 3))
(defn div4 [v] (/ v 4))
```

are the trivial improvement on the original example, allowing us to do

```
user> (let [f (comp div4 add3 mult2)]
        (f 50))
103/4
```

instead. The definitions of `mult3` and `add3` can be made more succinct using 'point-free' form
using `partial` thus avoiding the introductrion of a named variable:

```
(def mult2 (partial * 2))
(def add3  (partial + 3))
(def div4  #(/ % 4))
```

Notice that, since `partial` will add arguments to the *end*, we must use different style of definition for
`div4`. The above definitions are equivalent to the originals:

```
user> (let [f (comp div4 add3 mult2)]
        (f 50))
103/4
```

The variant definition `div4` seems quite arbitrary and depends on the specific functionality of
`partial`. Clojure has operators which want their operand(s) at the start of the argument list, and
thus fit comfortably inside `->` forms, and operators which want their operand(s) at end end of the
end of the argument list, and thus fit comfortably inside `->>` forms.  Clojure offers `partial`
which caters to the latter case, but lacks an equivalent operator catering to the former.

`compose` strives to make using functions-as-values and composition easier by providing operators
which eliminate variations such as that between `mult2`/`add3` and `div4`. It also provides
short-hand notation for some of the operators, allowing very succinct expression (at the price of
having to remember the notation!).

`compose` provides, for example, `partial1` which supplements `partial` by returning a function
which a single value to the argument list before calling the wrapped function.

This allows these more consistent definitions

```
(def mult2 (partial * 2))
(def add3  (partial + 3))
(def div4  (partial1 / 4))
```

For succinctness, `compose` additionally provides the namespace `io.simplect.compose.notation` which
adds short names (greek letters) for some the operators. Specifically, `notation` defines `Π` (aka
`GREEK CAPITAL LETTER PI` in Unicode) to mean `partial` and `π` (`GREEK SMALL LETTER PI`) to mean
`partial1`.  This allows the above definitions to be written equivalently as

```
(def mult2 (Π * 2))
(def add3  (Π + 3))
(def div4  (π / 4))
```

`notation` defines `Γ`  (`GREEK CAPITAL LETTER GAMMA`) to mean `rcomp` and `γ` to (`GREEK SMALL
LETTER GAMMA`) to mean `comp`, allowing us to define the original operation as

```
(def op (γ div4 add3 mult2))
```

or, equivalently and perhaps more intuitively, as

```
(def op (Γ mult2 add3 div4))
```

or perhaps simply as

```
(def op (Γ (Π * 2) (Π + 3) (π / 4)))
```

although this form lumps the individual operations back together but illustrates the succinctness of
point-free function definitions and the operators of `notation`.

In many situations using `rcomp` thus applying functions in the order they appear makes the
expression easier to understand than using `comp` which applies them in reverse order.

To close off the example the definitions

```
(def mult2 (π * 2))
(def add3  (π + 3))
(def div4  (π / 4))

(def op (Γ mult2 add3 div4))
```

seem to strike an attractive balance, with clear code structure, the ability to test each part
separately and retaining the possibility of reusing each constituent part. (The definitions of
`mult2` and `add3` were adjusted to use `π` to eliminate arbitrary, unnecessary variation.) In this
case avoiding named arguments appears to help keep the code simple and easy to understand.

`compose` strives to make it easier to program like this.

### PS

`compose` makes no claims of originality.  It merely assembles functionality the author has found
useful in his own quest towards a more functional programming style, some of which is lifted from
[Clojure standard libraries](https://github.com/clojure/algo.generic) (`fmap`) or [Funcool
cats](http://funcool.github.io/cats/latest/) (`curry`).  (Thanks!)
