module Main where

-- import Render (app)
import XYZ (whitePointFromKelvin, desaturate, printMatrix)
import Prelude

import Effect (Effect)
import Effect.Class.Console (log)

main :: Effect Unit
main = do
    let kelvin = 5000 -- Example color temperature
    let whitePoint = whitePointFromKelvin kelvin

    putStrLn $ "White Point for " ++ show kelvin ++ "K:"
    printMatrix whitePoint

    -- Define the original color (for desaturation example)
    let x = 0.54
    let y = 0.362
    let originalColor = fromList 2 1 [x, y]

    -- Define desaturation factor
    let alpha = 0.75 -- 25% desaturation

    putStrLn "\nOriginal Color:"
    printMatrix originalColor

    let desaturatedColor = desaturate alpha originalColor whitePoint
    putStrLn "\nDesaturated Color:"
    printMatrix desaturatedColor
