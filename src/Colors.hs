{-# LANGUAGE DeriveGeneric,  OverloadedStrings #-}

module Colors where

import qualified Data.Text as T
import Utility (varName)

genColorDeclararions :: [(T.Text, T.Text)] -> T.Text
genColorDeclararions ls = foldl (\ acc val -> 
    case val of 
        (a, _) -> case acc of
            "" -> acc <> "." <> a <> " {\n" <> "  color: " <> (varName a) <> ";\n}"
            _ -> acc <> "\n\n." <> a <> " {\n" <> "  color: " <> (varName a) <> ";\n}"
    ) 
    ("" :: T.Text) 
    ls