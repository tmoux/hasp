- Update token parser
- Use TH to make typesafe parsers
  - Problem with instances for GADTs
  - Might have to update the Grammar to take in a quoted function
- Do benchmarks with parsec, other parsing libraries
- Improve parsing errors
- Add more combinators
- Switch around parameters in Grammar

### Eventually:
- Add staging with TH

- Improve error reporting

Multiple "backends": custom parser combinator, convert into parsec, use TH to generate more efficient code/erase types

Can't be worse than just converting to Parsec combinators right?