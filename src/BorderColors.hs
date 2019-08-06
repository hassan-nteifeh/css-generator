{-# LANGUAGE OverloadedStrings #-}

module BorderColors where

import qualified Data.Text as T
import Types
import Utility (decInd)

varName :: T.Text -> T.Text
varName x = "var(--" <> x <> ")"

genBrClrRule :: Color -> T.Text
genBrClrRule (name, _) = 
  ".b--" <> name
  <> " {\n" <> decInd 
  <> "border-color: " 
  <> (varName name) <> ";\n}"

genBorderColorsCss :: [Color] -> T.Text
genBorderColorsCss xs = foldl (\acc x -> case acc of
    "" -> acc <> (genBrClrRule x)
    _ -> acc <> "\n\n" <> (genBrClrRule x)
    ) 
    ""
    xs
