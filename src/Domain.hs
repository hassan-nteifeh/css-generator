{-# LANGUAGE DeriveGeneric,  OverloadedStrings #-}

module Domain where

import Data.Aeson
import GHC.Generics
import Numeric (showFFloat)
import Data.Typeable
import qualified Data.Scientific as DS
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as Map
import GHC.Exts    -- (fromList)

unwrap :: Value -> T.Text
unwrap (String val) = val

showFullPrecision :: Float -> String
showFullPrecision x = showFFloat Nothing x ""

createOpaClass :: Float -> [Char]
createOpaClass x = case x of
    0.0 -> ".o-0 {\n" ++ "opacity: " ++ showFullPrecision x ++ ";\n}"
    1.0 -> ".o-100 {\n" ++ "opacity: " ++ showFullPrecision x ++ ";\n}"
    _ -> let name = filter (\c -> c /= '.') $ "o-" ++ (showFullPrecision (x * 10))
      in "." ++ name ++ "{\n" ++ "opacity: " ++ showFullPrecision x ++ ";\n}"

generateOpaClasses :: [Float] -> [Char] 
generateOpaClasses = 
    foldl (\acc x -> case acc of
        "" -> acc ++ (createOpaClass x)
        _ -> acc ++ ("\n\n") ++ (createOpaClass x)) 
        ""

processColorData x =  map (\v -> (fst v, unwrap $ snd $ v)) x

generateColorsDeclarations :: [(T.Text, T.Text)] -> T.Text
generateColorsDeclarations ls = foldl (\ acc val -> case val of 
    (a, b) -> acc <> "\n\n." <> a <> "{\n" <> "\tcolor: " <> b <> ";\n}"
    ) 
    ("" :: T.Text) 
    ls
    
data Config = Config {
    opacity :: [Float],
    colors :: Object
} deriving (Show)

instance FromJSON Config where
    parseJSON = withObject "Config" $ \o -> do
        op <- o .: "opacity"
        cs <- o .: "colors"
        return $ Config op cs

instance ToJSON Config where
  toJSON config = object
    [ "opacity" .= toJSON (opacity config)
    , "colors" .= toJSON (colors config)
    ]