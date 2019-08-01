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

createOpaClass :: Float -> T.Text
createOpaClass x = case x of
    0.0 -> T.pack $ ".o-0 {\n" ++ "  opacity: " ++ showFullPrecision x ++ ";\n}"
    1.0 -> T.pack $ ".o-100 {\n" ++ "  opacity: " ++ showFullPrecision x ++ ";\n}"
    _ -> let name = (filter (\c -> c /= '.') $ "o-" ++ (showFullPrecision (x * 10)))
      in T.pack $ "." ++ name ++ "{\n" ++ "  opacity: " ++ showFullPrecision x ++ ";\n}"

generateOpaClasses :: [Float] -> T.Text 
generateOpaClasses = 
    foldl (\acc x -> case acc of
        "" -> acc <> (createOpaClass x)
        _ -> acc <> (T.pack "\n\n") <> (createOpaClass x)) 
        (T.pack "")

processColorData :: [(T.Text, Value)] -> [Color]
processColorData x =  map (\v -> (fst v, unwrap $ snd $ v)) x

parseColors :: Config -> [Color]
parseColors = processColorData . toList . colors 

createCSSVarDeclration :: (T.Text, T.Text) -> T.Text
createCSSVarDeclration (name, value) = "  --" <> name <> ": " <> value <> ";"

generateBgRule :: Bg -> T.Text
generateBgRule (name, _) = ".bg-" <> name <> " {\n" <> decInd <> "background-color: " <> (varName name) <> ";\n}"

type Color = (T.Text, T.Text)
type Bg = (T.Text, T.Text)

decInd = "  "

generateBgRules :: [Bg] -> T.Text
generateBgRules xs = foldl (\acc x -> case acc of
    "" -> acc <> (generateBgRule x)
    _ -> acc <> "\n\n" <> (generateBgRule x)
    ) 
    ""
    xs

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

varName :: T.Text -> T.Text
varName x = "var(--" <> x <> ")"

generateColorsDeclarations :: [(T.Text, T.Text)] -> T.Text
generateColorsDeclarations ls = foldl (\ acc val -> 
    case val of 
        (a, _) -> case acc of
            "" -> acc <> "." <> a <> " {\n" <> "  color: " <> (varName a) <> ";\n}"
            _ -> acc <> "\n\n." <> a <> " {\n" <> "  color: " <> (varName a) <> ";\n}"
    ) 
    ("" :: T.Text) 
    ls
    
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