Improved space consumers for megaparsec
===

This package provides an alternative approach for white space consuming to
[Text.Megaparsec.Char.Lexer](https://hackage.haskell.org/package/megaparsec-9.2.2/docs/Text-Megaparsec-Char-Lexer.html).

*WARNING this package is under implementation*. Bugs are very likely here. All
behavior is instable, don't rely on any version policy.

Available features
---
At the moment `Text.Megaparsec.Char.Lexer.New` provides combinators mostly like
standard megaparsec lexer does with the following differences:

- there is a special `newtype` for line space consumers and a type synonym for
  the space and eols consumers
- `indentBlock` replaced by a more flexible `block` and `headedBlock`
  combinators
- `lineFold` behavior changed

In general, new approach impose more restrictions on how white space should be
consumed. In my experience not enough clear rules leads here to bugs so I'd like
to restrict them. However, I don't think current implementation to be the
only right, so I'd like to consider any other proposals and maybe change the
lib's behavior.


Usage
---
Just import `Text.Megaparsec.Char.Lexer.New` instead of
`Text.Megaparsec.Char.Lexer` --- all you need is exported from there.

Contribution
---
Issues (bugs, feature requests or otherwise feedback) may be reported in the
[GitHub issue tracker](https://github.com/Lev135/space-consumers/issues).

Pull requests are also welcome.
