{-# LANGUAGE DeriveGeneric,  OverloadedStrings #-}

module Border where

import qualified Data.Text as T
import Types

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