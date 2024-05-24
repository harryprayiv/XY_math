# Dork Manager

[![Haskell CI using Nix Flake](https://github.com/cardanonix/pelotero-engine/actions/workflows/haskell.yml/badge.svg)](https://github.com/cardanonix/pelotero-engine/actions/workflows/haskell.yml)

To reverse engineer a desaturation algorithm in the XY colorspace using matrix math in Haskell, I will need to understand the mathematical principles behind color desaturation and how to represent these operations using matrices. Here's a step-by-step approach to tackle this problem:

Step 1: Understand the XY Colorspace
The XY colorspace typically refers to chromaticity coordinates (x, y) used in the CIE 1931 color space. These coordinates represent color information without considering luminance. Desaturation in this context means reducing the chromaticity while maintaining the hue (tint).

Step 2: Mathematical Representation
Desaturation can often be achieved by blending the color with a neutral grey or white point. This blending operation can be represented using linear interpolation. Mathematically, if 
(
𝑥
,
𝑦
)
(x,y) is your original color and 
(
𝑥
𝑤
,
𝑦
𝑤
)
(x 
w
​
 ,y 
w
​
 ) is the white point (typically 
(
0.333
,
0.333
)
(0.333,0.333) for D65 white point), the desaturated color 
(
𝑥
𝑑
,
𝑦
𝑑
)
(x 
d
​
 ,y 
d
​
 ) can be represented as:


# Contribute
Feel free to fork, improve, create pull requests, report bugs, or request new features.

Made with ❤️ by Harry Pray IV.
