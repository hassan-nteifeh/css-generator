{-# LANGUAGE OverloadedStrings #-}
module Main where 

import qualified Data.ByteString.Lazy as Lz
import Numeric (showFFloat)
import GHC.Exts    -- (fromList)
import Data.Typeable
import Data.Aeson (encode, eitherDecode)
import Data.Aeson.Types
import Domain
import Border
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.HashMap.Strict as HM


writeCssChunks :: [T.Text] -> String -> IO ()
writeCssChunks xs path = T.writeFile path $ constructCssChunks xs

constructCssChunks xs = foldl (\acc v -> case acc of
    "" -> acc <> v
    _ -> acc <> (T.pack "\n\n") <> v
    ) 
    (T.pack (""))
    xs

notNull x = not (T.null x)

main :: IO ()
main = do
    d <- eitherDecode <$> (Lz.readFile "./config.json") :: IO (Either String Config)
    case d of
        Left err -> fail err
        Right obj -> do
            let colorsL = parseColors obj
            let colorDeclarations = generateColorsDeclarations $ colorsL
            let opacityDeclarations = generateOpaClasses $ opacity obj
            let dt = filter notNull [genColorVarDeclarations colorsL]
            let bgRules = generateBgRules colorsL
            let colorDecs = genRootRule dt
            let brClrDecs = genBrClrRules colorsL
            writeCssChunks [colorDecs, bgRules, colorDeclarations, opacityDeclarations, brClrDecs] "./test.css"