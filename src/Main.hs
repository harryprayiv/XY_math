{-# LANGUAGE FlexibleContexts #-}

import Data.Matrix (Matrix, fromList, getElem, elementwise, nrows, ncols, transpose, multStd, identity)
import Data.List (foldl')

-- Function to perform polynomial regression and predict Y values
polynomialRegression :: Int -> [(Double, Double)] -> (Double -> Double)
polynomialRegression degree samples = \x -> sum $ zipWith (*) coeffs (map (x **) [0..])
  where
    (xs, ys) = unzip samples
    xs' = map (\x -> [x ** i | i <- [0..fromIntegral degree]]) xs
    xsMatrix = fromList (length xs) (degree + 1) (concat xs')
    ysMatrix = fromList (length ys) 1 ys
    coeffsMatrix = solveNormalEquations xsMatrix ysMatrix
    coeffs = toListMatrix coeffsMatrix

-- Helper function to solve the normal equation for polynomial regression
solveNormalEquations :: Matrix Double -> Matrix Double -> Matrix Double
solveNormalEquations x y = inv (transpose x `multStd` x) `multStd` transpose x `multStd` y

-- Function to invert a matrix
inv :: Matrix Double -> Matrix Double
inv m = case gaussJordan m (identity (nrows m)) of
          Just invM -> invM
          Nothing   -> error "Matrix is singular and cannot be inverted"

-- Gauss-Jordan elimination for matrix inversion
gaussJordan :: Matrix Double -> Matrix Double -> Maybe (Matrix Double)
gaussJordan a b
  | nrows a /= ncols a = Nothing
  | otherwise = go a b 0
  where
    n = nrows a
    go a b i
      | i >= n = Just b
      | otherwise = 
          let pivot = getElem (i+1) (i+1) a
              a' = scaleRow (1 / pivot) i a
              b' = scaleRow (1 / pivot) i b
              rows = [0..n-1]
              (a'', b'') = foldl' (eliminate i) (a', b') (filter (/= i) rows)
          in go a'' b'' (i + 1)
    eliminate i (a, b) j =
      let factor = getElem (j+1) (i+1) a
          a' = addRows (-factor) i j a
          b' = addRows (-factor) i j b
      in (a', b')
    scaleRow factor i m = fromList (nrows m) (ncols m) [if r == i then factor * getElem (r+1) (c+1) m else getElem (r+1) (c+1) m | r <- [0..nrows m - 1], c <- [0..ncols m - 1]]
    addRows factor i j m = fromList (nrows m) (ncols m) [if r == j then getElem (r+1) (c+1) m + factor * getElem (i+1) (c+1) m else getElem (r+1) (c+1) m | r <- [0..nrows m - 1], c <- [0..ncols m - 1]]

-- Sample data for white points and their corresponding XYZ coordinates
whitePointSamples :: [(Double, (Double, Double))]
whitePointSamples = 
    [ (2000, (0.527, 0.413))
    , (2100, (0.516, 0.415))
    , (2400, (0.486, 0.415))
    , (2600, (0.468, 0.412))
    , (2800, (0.452, 0.408))
    , (3000, (0.437, 0.404))
    , (3200, (0.423, 0.399))
    , (4000, (0.380, 0.377))
    , (5000, (0.345, 0.352))
    , (6000, (0.322, 0.332))
    , (7000, (0.306, 0.316))
    , (8000, (0.295, 0.305))
    , (9000, (0.287, 0.295))
    , (10000, (0.281, 0.288))
    , (20000, (0.256, 0.258))
    ]

-- Fit polynomials to the sample data
fitPolynomials :: Int -> [(Double, (Double, Double))] -> (Double -> Double, Double -> Double)
fitPolynomials degree samples = (fittedX, fittedY)
  where
    (temps, coords) = unzip samples
    (xs, ys) = unzip coords
    fittedX = polynomialRegression degree (zip temps xs)
    fittedY = polynomialRegression degree (zip temps ys)

-- Function to get polynomial functions for XYZ coordinates
getFittedFunctions :: Int -> [(Double, (Double, Double))] -> (Double -> Double, Double -> Double)
getFittedFunctions degree samples = (fittedX, fittedY)
  where
    (fittedX, fittedY) = fitPolynomials degree samples

-- Function to get XYZ from Kelvin using polynomial regression
kelvinToXY :: Double -> (Double, Double)
kelvinToXY kelvin = (fittedX kelvin, fittedY kelvin)
  where
    (fittedX, fittedY) = getFittedFunctions 5 whitePointSamples  -- Increased degree to 5

-- Function to calculate white point from Kelvin temperature using polynomial regression
whitePointFromKelvin :: Double -> Matrix Double
whitePointFromKelvin kelvin = fromList 2 1 [xw, yw]
  where
    (xw, yw) = kelvinToXY kelvin

-- Desaturation function
desaturate :: Double -> Matrix Double -> Matrix Double -> Matrix Double
desaturate alpha color white = customElementwise (+) (scaleMatrix alpha color) (scaleMatrix (1 - alpha) white)

-- Helper function to scale a matrix by a scalar
scaleMatrix :: Double -> Matrix Double -> Matrix Double
scaleMatrix scalar matrix = fromList (nrows matrix) (ncols matrix) $ map (* scalar) (toListMatrix matrix)

-- Helper function to convert matrix to list
toListMatrix :: Matrix Double -> [Double]
toListMatrix mat = [ getElem i 1 mat | i <- [1..nrows mat] ]

-- Custom elementwise function to avoid conflict
customElementwise :: (Double -> Double -> Double) -> Matrix Double -> Matrix Double -> Matrix Double
customElementwise f m1 m2 = fromList (nrows m1) (ncols m1) $ zipWith f (toListMatrix m1) (toListMatrix m2)

-- Helper function to print matrix elements
printMatrix :: Matrix Double -> IO ()
printMatrix mat = do
    putStrLn $ "x: " ++ show (getElem 1 1 mat)
    putStrLn $ "y: " ++ show (getElem 2 1 mat)

main :: IO ()
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
