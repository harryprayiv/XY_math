import Data.Matrix (Matrix, fromList, multStd, getElem)

-- Define the original color and white point
originalColor :: Matrix Double
originalColor = fromList 2 1 [x, y]

whitePoint :: Matrix Double
whitePoint = fromList 2 1 [xw, yw]

-- Desaturation function
desaturate :: Double -> Matrix Double -> Matrix Double -> Matrix Double
desaturate alpha color white = (alpha `scaleMatrix` color) + ((1 - alpha) `scaleMatrix` white)

-- Helper function to scale a matrix by a scalar
scaleMatrix :: Double -> Matrix Double -> Matrix Double
scaleMatrix scalar matrix = fromList 2 1 $ map (* scalar) (toList matrix)

-- Example usage
main :: IO ()
main = do
    let alpha = 0.5 -- 50% desaturation
    let desaturatedColor = desaturate alpha originalColor whitePoint
    print desaturatedColor

-- Helper function to convert matrix to list
toList :: Matrix Double -> [Double]
toList mat = [ getElem i 1 mat | i <- [1..2] ]
