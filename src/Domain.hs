{-# LANGUAGE DeriveGeneric,  OverloadedStrings #-}

module Domain where

import Data.Aeson
import GHC.Generics
import Data.Typeable
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as Map
import GHC.Exts    -- (fromList)

unwrap :: Value -> T.Text
unwrap (String val) = val

createOpaClass :: Double -> [Char]
createOpaClass x = case x of
    0.0 -> ".op-0 {\n" ++ "opacity: " ++ show x ++ ";\n}"
    1.0 -> ".op-1 {\n" ++ "opacity: " ++ show x ++ ";\n}"
    _ -> let name = filter (\c -> c /= '.') $ "op-" ++ (show x)
      in "." ++ name ++ "{\n" ++ "opacity: " ++ show x ++ ";\n}"

generateOpaClasses :: [Double] -> [Char] 
generateOpaClasses = 
    foldl (\acc x -> acc ++ "\n\n" ++ (createOpaClass x)) ""

processColorData x =  map (\v -> (fst v, unwrap $ snd $ v)) x

generateColorsDeclarations :: [(T.Text, T.Text)] -> T.Text
generateColorsDeclarations ls = foldl (\ acc val -> case val of 
    (a, b) -> acc <> "\n\n." <> a <> "{\n" <> "\tcolor: " <> b <> ";\n}"
    ) 
    ("" :: T.Text) 
    ls
    
data Config = Config {
    opacity :: [Double],
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