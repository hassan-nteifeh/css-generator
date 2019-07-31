{-# LANGUAGE OverloadedStrings #-}
module Main where 

import qualified Data.ByteString.Lazy as Lz
import Numeric (showFFloat)
import GHC.Exts    -- (fromList)
import Data.Typeable
import Data.Aeson (encode, eitherDecode)
import Data.Aeson.Types
import Domain (Config, generateOpaClasses, opacity, colors, processColorData, generateColorsDeclarations)
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

main :: IO ()
main = do
    d <- eitherDecode <$> (Lz.readFile "./config.json") :: IO (Either String Config)
    case d of
        Left err -> fail err
        Right ps -> do
            let colorDeclarations = generateColorsDeclarations $ processColorData $ toList $ colors ps
            let opacityDeclarations = generateOpaClasses $ opacity ps
            putStrLn $ show $ T.length colorDeclarations
            putStrLn $ show $ typeOf opacityDeclarations
            writeCssChunks [colorDeclarations, opacityDeclarations] "./test.css"
            putStrLn $ T.unpack $ constructCssChunks [colorDeclarations, opacityDeclarations]
            putStrLn "Meh"