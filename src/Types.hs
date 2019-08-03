{-# LANGUAGE DeriveGeneric,  OverloadedStrings #-}

module Types where

import qualified Data.Text as T

type Color = (T.Text, T.Text)
type Bg = (T.Text, T.Text)
--                 name,   min, max
type Breakpoint = (T.Text, Int, Maybe Int)