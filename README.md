## hasp

hasp is a parser combinator library that aims to help write parsers for unambiguous, context-free grammars that run in linear time.
It is based off the paper [A Typed, Algebraic Approach to Parsing](https://www.cl.cam.ac.uk/~jdy22/papers/a-typed-algebraic-approach-to-parsing.pdf) and its artifact [ocaml-asp](https://github.com/yallop/ocaml-asp/).
hasp adds a type system to grammars, allowing it to reject grammars with ambiguity or left-recursion.
hasp also allows its parsers to be "implemented" in several different ways, including using staged compilation (Template Haskell) to erase types and generate more efficient parsers.

## Usage

Using hasp should feel familiar to anyone familiar with `parsec` or similar parser combinator libraries.
Most standard [combinators](https://hackage.haskell.org/package/parsec-3.1.17.0/docs/Text-Parsec-Combinator.html) are provided.
The main difference is that recursive parsers are defined with the fixpoint operator `fix` and higher-order abstract syntax:



Note that while `Hoas t` is an Alternative Applicative, as all proper parser combinators should be, it is not a monad, and for good reason: implementing monadic bind would prevent us from performing the static analysis that allows to have such unambiguity guarantees.

## Implementation details