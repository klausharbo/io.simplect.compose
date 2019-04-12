# Notation

`compose` includes the namespace `io.simplect.compose.notation` providing a single Greek letter as
an abbreviation.

The table below provides an overview of each abbreviated form (`core` refers to `clojure.core`,
`compose` to `io.simplect.compose`):

|--------------|--------------------|---------------------------|
| **Notation** | **Abbreviates**    | **Intended mnemonic**     |
| γ            | `core/comp`        | 'c' (gamma) for *compose* |
| Γ            | `compose/rcomp`    | 'C' (Gamma) for *compose* |
| π            | `compose/partial1` | 'p' (pi) for *partial*    |
| Π            | `core/partial`     | 'P' (Pi) for *partial*    |
| χ            | `compose/curry`    | 'k' (chi) for *kurry*     |
| λ            | `core/fn`          | 'lambda' for *lambda*     |
| μ            | `core/map`         | 'm' (mu) for *map*        |
| ρ            | `core/reduce`      | 'r' (rho) for *reduce*    |

Some people may prefer to not use these abbreviated forms, which require you (1) to remember the
meaning of each and (2) may not be straightforward to enter on most keyboards.  

Different computing environments offer different solutions to the character entry issue, below is a
sketch of how to use Abbrevs in Emacs.

The author has rebound his keyboard to easily enter any Greek character, Emacs e.g. allows `M-x
set-input-method RET TeX` enabling you to type `\beta` to get `β`, or allows you to define `Abbrevs`
substituting input as you type it.

Since `io.simplect.compose.notation` is merely notation, you can opt to not use it without losing
any of the functionality of `compose`.

## Using the notation

The notation characters work best if they can be used without a namespace qualifier, either by
simply requiring all of them

```
(ns myns
  (:require [io.simplect.compose.notation	:refer :all]))
```

or importing them individually as needed

```
(ns myns
  (:require [io.simplect.compose.notation	:refer [π Π γ Γ]]))
```

## Defining Abbrevs in Emacs

If you use Emacs one way to enter the Greek characters is use Emacs' *Abbrevs*.  Here's a brief
introduction to defining abbreviations for the notation characters:

1. Go to buffer in `clojure-mode` byffer (since we'll be defining mode-specific abbreviations)
1. Do `M-x set-input-method RET TeX RET` to enable you to use TeX syntax for entering the characters
1. Do `M-x abbrev-mode` to activate replacement of abbreviations
1. Type `\Gamma` (immediately replaced by `Γ`)
1. `M-x add-mode-abbrev RET Gamma RET`

Repeat for last two steps for remaining notation characters.

If you want use *Abbrevs* to enter the notation characters you'll need to activate it by adding it
to the mode-hook for `clojure-mode`:

```
(add-hook clojure-mode-hook #'abbrev-mode)
```
