{ name = "doffy"
, dependencies =
  [ "arrays"
  , "ask"
  , "canvas"
  , "fixed-points"
  , "free"
  , "matryoshka"
  , "maybe"
  , "prelude"
  , "profunctor-lenses"
  , "sized-vectors"
  , "tuples"
  , "typelevel"
  , "undefined-is-not-a-problem"
  , "unordered-collections"
  , "web-uievents"
  , "zipperarray"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "GPL-3.0-or-later"
, repository = "https://github.com/Mateiadrielrafael/purescript-ask"
}
