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

genBrClrRules :: [Color] -> T.Text
genBrClrRules xs = foldl (\acc x -> case acc of
    "" -> acc <> (genBrClrRule x)
    _ -> acc <> "\n\n" <> (genBrClrRule x)
    ) 
    ""
    xs


elemIndex' x xs = unwrap $ elemIndex x xs 

createBPStartBlock :: Breakpoint -> T.Text
createBPStartBlock (_, b, Just c) = "@media (min-width: " <> (T.pack $ show b) <> "em) and (max-width: calc(" <> (T.pack $ show c) <> "em - 1px)) {"
createBPStartBlock (_, b, Nothing) = "@media (min-width: " <> (T.pack $ show b) <> "em) {"

fst' (a, b, c) = a

genBWRule :: Int -> Float -> T.Text -> T.Text
genBWRule l n t = 
    case t of 
        "" -> ".bw" <> (showInt l) 
              <> " {\n" <> decInd 
              <> "border-width: " 
              <> (showFloat n) <> "rem;\n}"
        _ -> ".bw" <> (showInt l) <> "-" <> t
             <> " {\n" <> decInd 
             <> "border-width: " 
             <> (showFloat n) <> "rem;\n}"

genBPRule ::  [Float] -> Breakpoint -> T.Text
genBPRule xs bp = 
    let startBlock = createBPStartBlock bp
        rules = foldl (\acc x -> 
            let idx =  elemIndex' x xs
            in acc <> "\n\n" <> genBWRule idx x (fst' bp)
            ) 
            ""
            xs 
    in startBlock <> rules <> "\n\n}"

genBWBPRules :: [Float] -> [Breakpoint] -> T.Text
genBWBPRules xs bps = foldl (\acc bp -> 
    acc <> (genBPRule xs bp) <> "\n\n\n"
    ) 
    ""
    bps

genBWRulesNoBP ::  [Float] -> T.Text
genBWRulesNoBP xs = foldl (\acc x -> 
    let idx =  elemIndex' x xs
    in acc <> "\n\n" <> genBWRule idx x ""
    ) 
    ""
    xs

genBWRules :: [Float] -> [Breakpoint] -> T.Text
genBWRules xs bps = genBWRulesNoBP xs <> "\n\n" <> genBWBPRules xs bps