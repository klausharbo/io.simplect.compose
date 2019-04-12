# Reordering arguments

Clojure both uses functions which need the value they operate on to be the last argument (like
`map`) and functions which need the vault they operate on to be the first argument (like `assoc` and
protocol functions working on reified objects).  `compose` provides functions reordering arguments
to make composing functions of both styles together.

## `>>->`

`>>->` reorders function arguments to make a function needing arguments at the start to fit into a
context providing the arguments at the end:

```
user> (->> {:a 1, :b 2}
           (>>-> assoc :c 9))
{:a 1, :b 2, :c 9}
```

## `>->>`

`>->>` reorders function arguments to make a function needing arguments at the start to fit into a
context providing the argument first:

```
user> (-> {:a 1, :b 2}
          (>->> map str))
("[:a 1]" "[:b 2]")
```

