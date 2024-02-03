- Do benchmarks with parsec, other parsing libraries
- Use TH to make typesafe parsers
  - Problem with instances for GADTs
  - Might have to update the Grammar to take in a quoted function
- Improve parsing errors
- Add more combinators
- Switch around parameters in Grammar
- Re-export Data.GADT stuff to clean up imports
- Clean up language extensions with default-extensions in .cabal

### Eventually:
- Add staging with TH

- Improve error reporting
- EOF combinator? notFollowedBy?
  - Would need to know all the possible tokens?

Multiple "backends": custom parser combinator, convert into parsec, use TH to generate more efficient code/erase types

Can't be worse than just converting to Parsec combinators right?
Doesn't exactly work since we are using our token type
