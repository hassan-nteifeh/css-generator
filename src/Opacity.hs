{-# LANGUAGE DeriveGeneric,  OverloadedStrings #-}

module Opacity where

import qualified Data.Text as T
import Utility (showFullPrecision)

genOpacityRule :: Float -> T.Text
genOpacityRule x = case x of
    0.0 ->T.pack $ ".o-0 {\n" <> "  opacity: " <> showFullPrecision x <> ";\n}"
    1.0 -> T.pack $ ".o-100 {\n" <> "  opacity: " <> showFullPrecision x <> ";\n}"
    _ -> let name = (filter (\c -> c /= '.') $ "o-" <> (showFullPrecision (x * 10)))
      in T.pack $ "." <> name <> " {\n" <> "  opacity: " <> showFullPrecision x <> ";\n}"

genOpacityCss :: [Float] -> T.Text 
genOpacityCss = 
    foldl (\acc x -> case acc of
        "" -> acc <> (genOpacityRule x)
        _ -> acc <> "\n\n" <> (genOpacityRule x)) 
        ("")