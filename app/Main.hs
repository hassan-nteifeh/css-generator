{-# LANGUAGE OverloadedStrings #-}
module Main where 

import qualified Data.ByteString.Lazy as Lz
import qualified Data.ByteString as B
import GHC.Exts    -- (fromList)
import Data.Typeable
import Data.Aeson (encode, eitherDecode)
import Data.Aeson.Types
import Domain (Config, generateOpaClasses, opacity, colors, processColorData, generateColorsDeclarations)
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.HashMap.Lazy as Map

import GHC.Exts    -- (fromList)

val :: Value
val = Object $ fromList [
  ("numbers", Array $ fromList [Number 1, Number 2, Number 3]),
  ("boolean", Bool True) ]

main :: IO ()
main = do
    d <- eitherDecode <$> (Lz.readFile "./config.json") :: IO (Either String Config)
    T.putStrLn . T.decodeUtf8 . encode $ val
    case d of
        Left err -> putStrLn err
        Right ps -> do
            putStrLn $ show $ length $ toList $ colors ps
            --T.writeFile "test.css" $ generateColorsDeclarations $ processColorData $ toList $ colors ps
            putStrLn $ generateOpaClasses $ opacity ps