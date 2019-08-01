{-# LANGUAGE DeriveGeneric,  OverloadedStrings #-}

module Variables where

import qualified Data.Text as T

createCSSVarDeclration :: (T.Text, T.Text) -> T.Text
createCSSVarDeclration (name, value) = "  --" <> name <> ": " <> value <> ";"

genColorVarDeclaration = createCSSVarDeclration

genColorVarDeclarations :: [(T.Text, T.Text)] -> T.Text
genColorVarDeclarations [] = ""
genColorVarDeclarations xs = foldl (\acc v -> 
    case acc of
    "" -> genColorVarDeclaration v
    _ -> acc <> "\n" <> genColorVarDeclaration v
    ) "" xs

genRootRule :: [T.Text] -> T.Text
genRootRule [] = ""
genRootRule declarations = ":root {\n" <> (foldl (\acc v ->  acc <> v) "" declarations) <> "\n}"