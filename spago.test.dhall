let main = ./spago.dhall

in  { name = "doffy-tests"
    , dependencies = main.dependencies # [ "effect", "spec", "prelude", "aff" ]
    , sources = main.sources # [ "test/**/*.purs" ]
    , packages = ./packages.dhall
    }
