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


main :: IO ()
main = do
    d <- eitherDecode <$> (Lz.readFile "./config.json") :: IO (Either String Config)
    putStrLn $ showFullPrecision (0.0005 :: Float)
    case d of
        Left err -> putStrLn err
        Right ps -> do
            putStrLn $ show $ length $ toList $ colors ps
            --T.writeFile "test.css" $ generateColorsDeclarations $ processColorData $ toList $ colors ps
            T.writeFile "test.css"  $ T.pack $ generateOpaClasses $ opacity ps