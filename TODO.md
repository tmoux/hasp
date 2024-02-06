- Re-export Data.GADT stuff to clean up imports
- Clean up language extensions with default-extensions in .cabal
- Add CI to github
- Add more combinators
- Improve parsing errors

### Eventually:
- Use TH to make typesafe parsers
  - Problem with instances for GADTs
  - Might have to update the Grammar to take in a quoted function
- Add staging with TH

- Improve error reporting
- EOF combinator? notFollowedBy?
  - Would need to know all the possible tokens?

Multiple "backends": custom parser combinator, use TH to generate more efficient code/erase types

Can't be worse than just converting to Parsec combinators right (once we get rid of the types and set membership checks)
Doesn't exactly work since we are using our token type
