import Data.Matrix (Matrix, fromList, getElem, elementwise, nrows, ncols)

-- Define the original color
x, y :: Double
x = 0.54
y = 0.362

originalColor :: Matrix Double
originalColor = fromList 2 1 [x, y]

xw, yw :: Double
xw = 0.345
yw = 0.352

whitePoint :: Matrix Double
whitePoint = fromList 2 1 [xw, yw]

-- Desaturation function
desaturate :: Double -> Matrix Double -> Matrix Double -> Matrix Double
desaturate alpha color white = customElementwise (+) (scaleMatrix alpha color) (scaleMatrix (1 - alpha) white)

-- New function to calculate required light color to achieve target color through gel
colorTarget :: Double -> Matrix Double -> Matrix Double -> Matrix Double
colorTarget beta target gel = customElementwise (/) (customElementwise (-) target (scaleMatrix beta gel)) (fromList 2 1 [1 - beta, 1 - beta])

-- Helper function to scale a matrix by a scalar
scaleMatrix :: Double -> Matrix Double -> Matrix Double
scaleMatrix scalar matrix = fromList (nrows matrix) (ncols matrix) $ map (* scalar) (toList matrix)

-- Helper function to convert matrix to list
toList :: Matrix Double -> [Double]
toList mat = [ getElem i 1 mat | i <- [1..nrows mat] ]

-- Custom elementwise function to avoid conflict
customElementwise :: (Double -> Double -> Double) -> Matrix Double -> Matrix Double -> Matrix Double
customElementwise f m1 m2 = fromList (nrows m1) (ncols m1) $ zipWith f (toList m1) (toList m2)

-- Helper function to print matrix elements
printMatrix :: Matrix Double -> IO ()
printMatrix mat = do
    putStrLn $ "x: " ++ show (getElem 1 1 mat)
    putStrLn $ "y: " ++ show (getElem 2 1 mat)

-- Main function with detailed output
main :: IO ()
main = do
    let alpha = 0.75 -- 25% desaturation
    let beta = 0.5 -- Example value for the 1 stop of gel influence

    -- Define the gel color
    let xd = 0.4
    let yd = 0.35

    let gelColor = fromList 2 1 [xd, yd]

    putStrLn "Original Color:"
    printMatrix originalColor
    putStrLn "\nWhite Point:"
    printMatrix whitePoint
    putStrLn $ "\nDesaturation Factor (alpha): " ++ show alpha
    putStrLn $ "\nGel Influence Factor (beta): " ++ show beta
    putStrLn "\nGel Color:"
    printMatrix gelColor

    let desaturatedColor = desaturate alpha originalColor whitePoint
    putStrLn "\nDesaturated Color:"
    printMatrix desaturatedColor

    putStrLn "\nUsing Desaturated Color as Target for Color Transformation"
    let requiredLightColor = colorTarget beta desaturatedColor gelColor
    putStrLn "\nRequired Light Color to achieve Desaturated Color through Gel:"
    printMatrix requiredLightColor
