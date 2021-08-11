{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "console"
  , "css"
  , "effect"
  , "halogen"
  , "halogen-css"
  , "prelude"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "halogen/**/*.purs", "test/**/*.purs" ]
}
