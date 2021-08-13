# Ask

Typeclass based implicit arguments. Think `Reader` but without the burden of doing everything inside do-notation.

Featured usecases:

- Passing a `CanvasRenderingContext2D` down a large tree

For the full documentation check out [pursuit](TODO)

## Example

```purescript
foo :: Ask Int => Int -> String
foo bar = show (ask + bar)

-- same as `(provide 3 foo) 4`
buzz :: String
buzz = provide 3 (foo 4)  -- 7

goo :: String
goo = provide 2 (show $ askFor _Int) -- passing a proxy in order to avoid a type annotation

blue :: Number
blue = provide 4.2 $ local floor (foo 3) -- 7
```

## Development

Building the package

```
spago build
```

Running the test suite

```
spago -x ./spago.test.dhall test
```

If you think a particular helper would be an useful addition, feel free to open an issue.
