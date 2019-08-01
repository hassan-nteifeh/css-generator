{-# LANGUAGE DeriveGeneric,  OverloadedStrings #-}

module Parser where

import Data.Aeson
import GHC.Generics
import Data.Typeable
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as Map
import GHC.Exts    -- (fromList)
import Utility (unwrap, showFullPrecision, decInd, varName)
import Types (Color, Bg)

processColorData :: [(T.Text, Value)] -> [Color]
processColorData x =  map (\v -> (fst v, unwrap $ snd $ v)) x

parseColors :: Config -> [Color]
parseColors = processColorData . toList . colors 

generateBgRule :: Bg -> T.Text
generateBgRule (name, _) = ".bg-" <> name <> " {\n" <> decInd <> "background-color: " <> (varName name) <> ";\n}"

generateBgRules :: [Bg] -> T.Text
generateBgRules xs = foldl (\acc x -> case acc of
    "" -> acc <> (generateBgRule x)
    _ -> acc <> "\n\n" <> (generateBgRule x)
    ) 
    ""
    xs

data Config = Config {
    opacity :: [Float],
    borderWidths :: [Float],
    colors :: Object
} deriving (Show)

instance FromJSON Config where
    parseJSON = withObject "Config" $ \o -> do
        op <- o .: "opacity"
        bw <- o .: "borderWidths"
        cs <- o .: "colors"
        return $ Config op bw cs

instance ToJSON Config where
  toJSON config = object
    [ "opacity" .= toJSON (opacity config)
    , "borderWidths" .= toJSON (borderWidths config)
    , "colors" .= toJSON (colors config)
    ]