# Gauche-srfi-115

[Gauche](http://practical-scheme.net/gauche/) port of [SRFI 115: Scheme Regular Expressions](http://srfi.schemers.org/srfi-115/).

## TODO

* support `grapheme`, `bog`, and `eog` patterns.
* add feature identifiers: `regexp-non-greedy`, `regexp-look-around`, `regexp-backrefs`, `regexp-unicode`
* optimize `or`-pattern if all sub-expressions are cset-sre

## REQUIREMENT

* Gauche 0.9.5 or later
* [Gauche-pp](https://github.com/leque/Gauche-pp)
  if you want to re-generate [srfi-115/internal/char-set.scm](./srfi-115/internal/char-set.scm)

## LICENSE

BSD 2-clause
