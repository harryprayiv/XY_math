import Data.Matrix (Matrix, fromList, getElem, elementwise)

-- Define the original color
x, y :: Double
x = 0.54
y = 0.362

originalColor :: Matrix Double
originalColor = fromList 2 1 [x, y]

-- Define the white point (assumed values for 5000K, need to be determined accurately)
xw, yw :: Double
xw = 0.34510 -- Approximate white point values for 5000K
yw = 0.35161

whitePoint :: Matrix Double
whitePoint = fromList 2 1 [xw, yw]

-- Desaturation function
desaturate :: Double -> Matrix Double -> Matrix Double -> Matrix Double
desaturate alpha color white = elementwise (+) (scaleMatrix alpha color) (scaleMatrix (1 - alpha) white)

-- Helper function to scale a matrix by a scalar
scaleMatrix :: Double -> Matrix Double -> Matrix Double
scaleMatrix scalar matrix = fromList 2 1 $ map (* scalar) (toList matrix)

-- Helper function to convert matrix to list
toList :: Matrix Double -> [Double]
toList mat = [ getElem i 1 mat | i <- [1..2] ]

-- Helper function to print matrix elements
printMatrix :: Matrix Double -> IO ()
printMatrix mat = do
    putStrLn $ "x: " ++ show (getElem 1 1 mat)
    putStrLn $ "y: " ++ show (getElem 2 1 mat)

-- Main function with detailed output
main :: IO ()
main = do
    let alpha = 0.75 -- 25% desaturation
    putStrLn "Original Color:"
    printMatrix originalColor
    putStrLn "\nWhite Point:"
    printMatrix whitePoint
    putStrLn $ "\nDesaturation Factor (alpha): " ++ show alpha

    let scaledOriginal = scaleMatrix alpha originalColor
    let scaledWhite = scaleMatrix (1 - alpha) whitePoint
    putStrLn "\nScaled Original Color:"
    printMatrix scaledOriginal
    putStrLn "\nScaled White Point:"
    printMatrix scaledWhite

    let desaturatedColor = desaturate alpha originalColor whitePoint
    putStrLn "\nDesaturated Color:"
    printMatrix desaturatedColor
