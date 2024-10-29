module XYZ where

import Prelude

import Data.Array (concat, fromFoldable, index, length, mapWithIndex, unzip, zip, zipWith, (..))
import Data.Foldable (sum)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (pow)
import Data.Tuple (Tuple(..))
import LinearAlgebra.Matrix (Matrix, add, fromArray, identity, inverse, multiply, ncols, nrows, rows, transpose, zeros)

-- Polynomial regression function for predictions
polynomialRegression :: Int -> Array (Tuple Number Number) -> (Number -> Number)
polynomialRegression degree samples = \x ->
  let
    Tuple xs ys = unzip samples
    xs' = concat $ map (\xVal -> map (\i -> pow xVal (fromIntegral i)) (0 .. degree)) xs
    xsMatrix = fromArray (length xs) (degree + 1) xs'
    ysMatrix = fromArray (length ys) 1 ys
    coeffsMatrix = solveNormalEquations xsMatrix ysMatrix
    coeffs = fromMaybe [] (fmap flattenMatrix coeffsMatrix)
  in sum $ zipWith (*) coeffs (map (\i -> pow x (fromIntegral i)) (0 .. degree))

-- Solve normal equations using matrix inversion and multiplication
solveNormalEquations :: Maybe (Matrix Number) -> Maybe (Matrix Number) -> Maybe (Matrix Number)
solveNormalEquations (Just x) (Just y) = do
  let xtx = multiply (transpose x) x
  xtxInv <- inverse xtx
  let xTy = multiply (transpose x) y
  pure $ multiply xtxInv xTy
solveNormalEquations _ _ = Nothing

-- Helper function to flatten a matrix to a single Array for output
flattenMatrix :: Matrix Number -> Array Number
flattenMatrix mat = concat $ rows mat

-- Sample data for white points and their corresponding XYZ coordinates
whitePointSamples :: Array (Tuple Number (Tuple Number Number))
whitePointSamples = fromFoldable
  [ Tuple 2000.0 (Tuple 0.527 0.413)
  , Tuple 2100.0 (Tuple 0.516 0.415)
  , Tuple 2400.0 (Tuple 0.486 0.415)
  , Tuple 2600.0 (Tuple 0.468 0.412)
  , Tuple 2800.0 (Tuple 0.452 0.408)
  , Tuple 3000.0 (Tuple 0.437 0.404)
  , Tuple 3200.0 (Tuple 0.423 0.399)
  , Tuple 4000.0 (Tuple 0.380 0.377)
  , Tuple 5000.0 (Tuple 0.345 0.352)
  , Tuple 6000.0 (Tuple 0.322 0.332)
  , Tuple 7000.0 (Tuple 0.306 0.316)
  , Tuple 8000.0 (Tuple 0.295 0.305)
  , Tuple 9000.0 (Tuple 0.287 0.295)
  , Tuple 10000.0 (Tuple 0.281 0.288)
  , Tuple 20000.0 (Tuple 0.256 0.258)
  ]

-- Fit polynomials to the sample data
getFittedFunctions :: Int -> Array (Tuple Number (Tuple Number Number)) -> Tuple (Number -> Number) (Number -> Number)
getFittedFunctions degree samples =
  let
    Tuple temps coords = unzip samples
    Tuple xs ys = unzip coords
    fittedX = polynomialRegression degree (zip temps xs)
    fittedY = polynomialRegression degree (zip temps ys)
  in Tuple fittedX fittedY

-- Obtain XYZ from Kelvin using polynomial regression
kelvinToXY :: Number -> Tuple Number Number
kelvinToXY kelvin =
  let
    Tuple fittedX fittedY = getFittedFunctions 5 whitePointSamples
  in Tuple (fittedX kelvin) (fittedY kelvin)

-- Calculate white point from Kelvin temperature using polynomial regression
whitePointFromKelvin :: Number -> Maybe (Matrix Number)
whitePointFromKelvin kelvin =
  let
    Tuple xw yw = kelvinToXY kelvin
  in fromArray 2 1 [xw, yw]

-- Desaturation function that scales and adds matrices using `add` from LinearAlgebra.Matrix
desaturate :: Number -> Matrix Number -> Matrix Number -> Maybe (Matrix Number)
desaturate alpha color white =
  add (scaleMatrix alpha color) (scaleMatrix (1.0 - alpha) white)

-- Helper function to scale a matrix by a scalar value
scaleMatrix :: Number -> Matrix Number -> Matrix Number
scaleMatrix scalar matrix = fromMaybe (zeros (nrows matrix) (ncols matrix)) $ fromArray (nrows matrix) (ncols matrix) scaledValues
  where
    scaledValues = map (\x -> scalar * x) (concat (rows matrix))

-- Gauss-Jordan elimination to calculate the inverse
gaussJordan :: Matrix Number -> Matrix Number -> Maybe (Matrix Number)
gaussJordan matrixA matrixB
  | nrows matrixA /= ncols matrixA = Nothing
  | otherwise = go matrixA matrixB 0
  where
    n = nrows matrixA
    go matrixA matrixB i
      | i >= n = Just matrixB
      | otherwise =
          case element i i matrixA of
            Just pivot ->
              let matrixA' = scaleRow (1 / pivot) i matrixA
                  matrixB' = scaleRow (1 / pivot) i matrixB
                  rowsToUpdate = filter (\j -> j /= i) (0 .. (n - 1))
                  (matrixA'', matrixB'') = foldl (eliminate i) (matrixA', matrixB') rowsToUpdate
              in go matrixA'' matrixB'' (i + 1)
            Nothing -> Nothing

    scaleRow scalar row mat = fromMaybe mat $ fromArray (nrows mat) (ncols mat) scaledValues
      where
        scaledValues = concat $ mapWithIndex (\i rowList -> if i == row then map (* scalar) rowList else rowList) (rows mat)

    eliminate i (matrixA, matrixB) j =
      case element j i matrixA of
        Just factor ->
          let matrixA' = addRows (-factor) i j matrixA
              matrixB' = addRows (-factor) i j matrixB
          in (matrixA', matrixB')
        Nothing -> (matrixA, matrixB)

    addRows scalar sourceRow targetRow mat = fromMaybe mat $ fromArray (nrows mat) (ncols mat) newValues
      where
        sourceRowValues = fromMaybe [] (index sourceRow (rows mat))
        newValues = concat $ mapWithIndex (\i rowList ->
          if i == targetRow
          then zipWith (\a b -> a + scalar * b) rowList sourceRowValues
          else rowList) (rows mat)
