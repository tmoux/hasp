## hasp

hasp (better name pending) is a parser combinator library for writing parsing unambiguous, context-free grammars that run in linear time.
It is based off the paper [A Typed, Algebraic Approach to Parsing](https://www.cl.cam.ac.uk/~jdy22/papers/a-typed-algebraic-approach-to-parsing.pdf) and its artifact [ocaml-asp](https://github.com/yallop/ocaml-asp/).
hasp adds a type system to grammars, allowing it to ensure grammars can be parsed efficiently, without ambiguity or left-recursion.
hasp also allows its parsers to be "implemented" in several different ways, including using staged compilation (Template Haskell) to erase types and generate more efficient parsers.

Note that hasp is still an early-stage project and is very unstable at the moment.

## Usage

Using hasp should feel familiar to anyone familiar with `parsec` or similar parser combinator libraries.
Most standard [combinators](https://hackage.haskell.org/package/parsec-3.1.17.0/docs/Text-Parsec-Combinator.html) are provided.
The main difference is that recursive parsers are defined with the fixpoint operator `fix` and higher-order abstract syntax.

### Writing parsers
For instance, consider the Dyck language, the set of strings of balanced parentheses.
It is defined by the context-free grammar `S ::= (S)S | ε`.
Here is how a parser that parses a Dyck word and counts the number of pairs of parentheses:

```haskell
dyck :: Hoas CharTag Int
dyck = fix $ 
  \p -> (\x y -> x + y + 1) <$> between (char '(') (char ')') p <*> p
        <|> eps 0
```

Parsers have the type `Hoas t a`, where `t` is a GADT representing "tags" of the types of tokens, and `a` is the type of the parsing result. `CharTag` is the default tag for parsing streams of characters.

We can compare this to the equivalent definition in Parsec:
```haskell
dyck :: Parsec String () Int
dyck = (\x y -> x + y + 1) <$> between (char '(') (char ')') dyck <*> dyck 
      <|> return 0
```

The definitions are very similar, except that recursion is done via the `fix` combinator.

Note that while `Hoas t` is an Alternative Applicative, like any good parser combinator should be, it is not a monad, and for good reason: implementing monadic bind would prevent us from performing the static analysis that allows to have unambiguity guarantees.

### Checking parsers

Technically speaking, a value of `Hoas t a` is not itself a parser, but rather a HOAS representation of one.
It is converted into a first-order representation which is checked to ensure there is no ambiguity or left recursion.
At a high level, we have a function `typecheck :: Hoas t a -> Bool` that will return True if the parser is accepted, and false if the parser is rejected.
As an example of an invalid parser, consider this definition of the same Dyck parser:

```haskell
dyck :: Hoas CharTag Int
dyck = fix $ 
  \p -> (\x y -> x + y + 1) <$> p <*> between (char '(') (char ')') p
        <|> eps 0
```
Here, the grammar is left-recursive, and thus this definition will be rejected. If we tried to use the analogous definition in Parsec to parse a string, it would happily spin its wheels forever in an infinite loop.

### Running parsers
Here is how to convert our parser `dyck` into a function that will take a `String` as a parameter, and return the number of balanced parentheses in the parsed Dyck word, or `Nothing` if there was a parsing failure:

```haskell
countParens :: String -> Maybe Int
countParens s = fst <$> parse (makeParser dyck) s
```


## Implementation details