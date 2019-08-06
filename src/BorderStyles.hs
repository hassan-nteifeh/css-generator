{-# LANGUAGE OverloadedStrings #-}
module BorderStyles where

import qualified Data.Text as T
import Types
import Utility (decInd)

type Style = T.Text

styles = 
    [ "none", "hidden", "dotted"
    , "dashed", "solid", "double"
    , "groove", "ridge", "inset"
    , "outset"
    ]

fst' (a, b, c) = a

getBPSuffix :: Maybe Breakpoint  -> T.Text
getBPSuffix x = case x of
    Nothing -> ""
    Just (a, _, _) -> "-" <> a

genClassName :: Style -> Maybe Breakpoint -> T.Text
genClassName x y = ".b--" <> x <> getBPSuffix y

genBrStyleRule :: Style -> Maybe Breakpoint -> T.Text
genBrStyleRule style bp = 
  genClassName style bp
  <> " {\n" <> decInd 
  <> "border-style: " 
  <> style <> ";\n}"

genBorderStylesCss :: Maybe Breakpoint -> T.Text
genBorderStylesCss bp = foldl (\acc x -> acc <> "\n" <> (genBrStyleRule x bp)) "" styles

--genBrStyleBreakpointCss :: [Float] -> Maybe Breakpoint -> T.Text
