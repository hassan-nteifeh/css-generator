{-# LANGUAGE DeriveGeneric,  OverloadedStrings #-}

module Utility where

import Numeric (showFFloat)
import qualified Data.Text as T
import Data.Aeson.Types
import Types (Breakpoint)

decInd = "  " :: T.Text

lb = "\n" :: T.Text
lb2 = lb <> lb

showFullPrecision :: Float -> String
showFullPrecision x = showFFloat Nothing x ""

varName :: T.Text -> T.Text
varName x = "var(--" <> x <> ")"

unwrap :: Value -> T.Text
unwrap (String val) = val

unwrapInt :: Value -> Int
unwrapInt (Number val) = round (val) :: Int

createBPBlockStart :: Maybe Breakpoint -> T.Text
createBPBlockStart Nothing = "" 
createBPBlockStart (Just (_, b, Just c)) = "@media (min-width: " <> (T.pack $ show b) <> "em) and (max-width: calc(" <> (T.pack $ show c) <> "em - 1px)) {\n"
createBPBlockStart (Just (_, b, Nothing)) = "@media (min-width: " <> (T.pack $ show b) <> "em) {"

createBPBlockEnd :: Maybe Breakpoint -> T.Text
createBPBlockEnd Nothing = ""
createBPBlockEnd _ = "\n\n}"