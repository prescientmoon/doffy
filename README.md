# Doffy

Purescript based canvas rendering framework.

> Note: I am in the process of extracting the library from one of my side projects. At the moment this repo only contains a small part of it.

Notable features include:

- FRP based rendering with TEA-like actions
- svg-like event listeners
- flexbox-inspired layouting system
- multi-step rendernig, where one step can query metadata collected from previous steps:
  - local transform matrices
  - geometries
  - relative & absolute bounds

As a testament to the extensability of the library, both the `Padding` and the `Flex` components have been implemented without adding any new branches to the `Geometry` adt.

## Planned features

- layers:
  at the moment each render step represents a layer. I would to support layers nested down a larger tree.
- multi-canvas rendering:
  display data on more than one canvas for minimal re-renders
- hover-events (eg: onhoverstart, onhoverend). Right now hovering is pull based (the programmer asks the library what is curently being hovered over)

### Would be nice to have, no idea if I will ever get to it

- chrome extension adding a geometry inspector
- multiple back ends (svg, webgl, etc)

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
