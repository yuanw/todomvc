{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "affjax"
  , "console"
  , "css"
  , "effect"
  , "either"
  , "halogen"
  , "halogen-css"
  , "http-methods"
  , "prelude"
  , "psci-support"
  , "web-events"
  ]
, packages = ./packages.dhall
, sources = [ "halogen/**/*.purs", "test/**/*.purs" ]
}
