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
  [ "aff"
  , "aff-bus"
  , "arrays"
  , "avar"
  , "bifunctors"
  , "console"
  , "contravariant"
  , "control"
  , "datetime"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "foreign-generic"
  , "functions"
  , "identity"
  , "integers"
  , "lists"
  , "maybe"
  , "newtype"
  , "node-buffer"
  , "node-fs"
  , "node-fs-aff"
  , "node-streams"
  , "nonempty"
  , "ordered-collections"
  , "prelude"
  , "profunctor"
  , "random"
  , "record"
  , "refs"
  , "st"
  , "strings"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "undefined"
  , "unfoldable"
  , "unicode"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
