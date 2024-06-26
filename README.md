# A Haskell Module for CIE XYZ Transformations

[![Haskell CI using Nix Flake](https://github.com/harryprayiv/XY_math/actions/workflows/haskell.yml/badge.svg)](https://github.com/harryprayiv/XY_math/actions/workflows/haskell.yml)

A Haskell-based attempt to replicate X,Y color desaturation from chromaCalc



```Original Color:
x: 0.54
y: 0.362

White Point:
x: 0.345
y: 0.352

Desaturation Factor (alpha): 0.75

Scaled Original Color:
x: 0.405
y: 0.27149999999999996

Scaled White Point:
x: 8.625e-2
y: 8.8e-2

Desaturated Color:
x: 0.49125
y: 0.35949999999999993```

However, chromaCalc gives me the following answer:
```
Desaturated Color:
x: 0.493
y: 0.359
```
They're close but not entirely in parity.  It appears that I am using more accurate floating point arithmatic since it is really just a rounding error away from chromaCalc.

# Contribute
Feel free to fork, improve, create pull requests, report bugs, or request new features.

Made with ❤️ by Harry Pray IV.
