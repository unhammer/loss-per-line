{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           System.Environment
import           Data.Char
import           System.IO            (stderr, isEOF)
import qualified Data.Text.IO         as T
--
import Lib

main :: IO ()
main = do
  args <- getArgs
  let sep = "\t"
  case map (map toLower) args of
    ["wer"] -> lossPerLine WER sep
    ["cer"] -> lossPerLine CER sep
    _ -> T.hPutStrLn stderr "Expecting first argument to be CER or WER"
