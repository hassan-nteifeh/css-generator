{-# LANGUAGE DeriveGeneric,  OverloadedStrings #-}

module Border where

import qualified Data.Text as T
import Numeric (showFFloat)
import Types
import Data.List


isInt :: (RealFrac a) => a -> Bool
isInt x = x == fromInteger (round x)

unwrap (Just num) = num

validInt :: Float -> Bool
validInt x = (fromInteger (round x) :: Float) == x

showFullPrecision :: Float -> T.Text
showFullPrecision x = T.pack $ showFFloat Nothing x ""

showInt :: Int -> T.Text
showInt x = T.pack $ show x

showFloat :: Float -> T.Text
showFloat x | validInt x = T.pack $ show $ round x
            | otherwise =  showFullPrecision x

decInd = "  "

varName :: T.Text -> T.Text
varName x = "var(--" <> x <> ")"

genBrClrRule :: Color -> T.Text
genBrClrRule (name, _) = 
  ".b--" <> name
  <> " {\n" <> decInd 
  <> "border-color: " 
  <> (varName name) <> ";\n}"

genBWRule :: Int -> Float -> T.Text
genBWRule l n = 
  ".bw" <> (showInt l)
  <> " {\n" <> decInd 
  <> "border-width: " 
  <> (showFloat n) <> "rem;\n}"

genBrClrRules :: [Color] -> T.Text
genBrClrRules xs = foldl (\acc x -> case acc of
    "" -> acc <> (genBrClrRule x)
    _ -> acc <> "\n\n" <> (genBrClrRule x)
    ) 
    ""
    xs


elemIndex' x xs = unwrap $ elemIndex x xs 

genBWRules :: [Float] -> T.Text
genBWRules xs = foldl (\acc x -> 
    let idx =  elemIndex' x xs 
        isLast = elemIndex' x xs == length xs - 1 
        in case acc of
            "" -> acc <> (genBWRule idx x)
            _ -> acc <> "\n\n" <> (genBWRule idx x)
        ) 
    ""
    xs