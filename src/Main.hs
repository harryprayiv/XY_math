import Data.Matrix (Matrix, fromList, getElem, elementwise, nrows, ncols)

-- More accurate function to convert Kelvin temperature to CIE 1931 xy chromaticity coordinates
kelvinToXY :: Double -> (Double, Double)
kelvinToXY t
    | t >= 4000 && t <= 7000 = (xD, yD)
    | t > 7000 = (xH, yH)
    | otherwise = error "Temperature out of range"
  where
    -- Constants for t in [4000, 7000]
    xD = -4.6070e9 / t^3 + 2.9678e6 / t^2 + 0.09911e3 / t + 0.244063
    yD = -3.000 * xD^2 + 2.870 * xD - 0.275

    -- Constants for t in [7000, 25000]
    xH = -2.0064e9 / t^3 + 1.9018e6 / t^2 + 0.24748e3 / t + 0.237040
    yH = -3.000 * xH^2 + 2.870 * xH - 0.275

-- Define the original color
x, y :: Double
x = 0.54
y = 0.362

originalColor :: Matrix Double
originalColor = fromList 2 1 [x, y]

-- Function to calculate white point from Kelvin temperature
whitePointFromKelvin :: Double -> Matrix Double
whitePointFromKelvin kelvin = fromList 2 1 [xw, yw]
  where
    (xw, yw) = kelvinToXY kelvin

-- Desaturation function
desaturate :: Double -> Matrix Double -> Matrix Double -> Matrix Double
desaturate alpha color white = customElementwise (+) (scaleMatrix alpha color) (scaleMatrix (1 - alpha) white)

-- New function to calculate required light color to achieve target color through diffusion
colorTarget :: Double -> Matrix Double -> Matrix Double -> Matrix Double
colorTarget beta target diffusion = customElementwise (/) (customElementwise (-) target (scaleMatrix beta diffusion)) (fromList 2 1 [1 - beta, 1 - beta])

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
    let beta = 0.5 -- Example value for the diffusion influence

    -- Define the diffusion color
    let xd = 0.4
    let yd = 0.35
    let diffusionColor = fromList 2 1 [xd, yd]

    -- Calculate the white point from Kelvin temperature
    let kelvin = 5000 -- Example color temperature
    let whitePoint = whitePointFromKelvin kelvin

    putStrLn "Original Color:"
    printMatrix originalColor
    putStrLn "\nWhite Point:"
    printMatrix whitePoint
    putStrLn $ "\nDesaturation Factor (alpha): " ++ show alpha
    putStrLn $ "\nDiffusion Influence Factor (beta): " ++ show beta
    putStrLn "\nDiffusion Color:"
    printMatrix diffusionColor

    let desaturatedColor = desaturate alpha originalColor whitePoint
    putStrLn "\nDesaturated Color:"
    printMatrix desaturatedColor

    putStrLn "\nUsing Desaturated Color as Target for Color Transformation"
    let requiredLightColor = colorTarget beta desaturatedColor diffusionColor
    putStrLn "\nRequired Light Color to achieve Desaturated Color through Diffusion:"
    printMatrix requiredLightColor