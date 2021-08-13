let main = ./spago.dhall

in  { name = "ask-tests"
    , dependencies = main.dependencies # [ "effect", "spec", "prelude", "aff" ]
    , sources = main.sources # [ "test/**/*.purs" ]
    , packages = ./packages.dhall
    }
