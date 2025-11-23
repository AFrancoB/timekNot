{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "my-project"
, dependencies =
  [ "arrays"
  , "bifunctors"
  , "console"
  , "datetime"
  , "effect"
  , "either"
  , "enums"
  , "foldable-traversable"
  , "foreign"
  , "formatters"
  , "halogen"
  , "halogen-svg-elems"
  , "identity"
  , "integers"
  , "js-bigints"
  , "js-timers"
  , "lists"
  , "maybe"
  , "newtype"
  , "now"
  , "numbers"
  , "ordered-collections"
  , "parsing"
  , "partial"
  , "prelude"
  , "purescript-tempi"
  , "purescript-webdirt"
  , "quickcheck"
  , "rationals"
  , "record"
  , "refs"
  , "shuffle"
  , "strings"
  , "svg-parser"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
