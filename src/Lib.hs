{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Lib (Metric(..), lossPerLine) where

import qualified Data.Text.Metrics    as M
import qualified Data.Vector          as V
import qualified Data.Vector.Distance as V
import           Data.Vector.Distance (Params(..))
import           Data.Ratio
import           Data.Monoid          (Sum   (..))
import           Control.Monad        (unless)
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           Text.Printf          (printf)
import           System.IO            (stderr, isEOF)

-- | For now we support character-based levenshtein and word error
-- rate. Both are length-normalised, where 0.0 is no changes and 1.0
-- is all changes.
data Metric =
    CER
  | WER
  deriving Show

-- | Get the WER/CER per sep-separated line in input.
-- Only the two first fields are compared, the rest are appended as-is.
-- Loss/score is prepended using sep.
lossPerLine :: Metric -> T.Text -> IO ()
lossPerLine metric sep = do
  let go (!n) = unlessM isEOF $ do
        line <- T.getLine
        prettyPrint $ lossOneLine metric sep n line
        go (n+1)
  go 1


unlessM :: Monad m => m Bool -> m () -> m ()
unlessM m1 m2 = m1 >>= \b -> unless b m2


splitLine :: Text -> Int -> Text -> Either Text (Text, Text, [Text])
splitLine sep n t = case T.splitOn sep t of
    guess:gold:rest -> Right (guess, gold, rest)
    _               -> Left $ "Line " <> T.pack (show n)
                        <> ": Couldn't find at least two parts (using '"<>sep<>"' as separator)"
                        <> " in “" <> t <> "”"


lossOneLine :: Metric -> Text -> Int -> Text -> Either Text (Double, Text, Text, [Text])
lossOneLine metric sep n line = fmap (withLoss metric) (splitLine sep n line)

-- Truncate if < 0.00001
maxLossLen :: Int
maxLossLen = 7

prettyPrint :: Either Text (Double, Text, Text, [Text]) -> IO ()
prettyPrint (Left msg) = T.hPutStrLn stderr $ "WARNING: " <> msg
prettyPrint (Right (m, guess, gold, rest)) = T.putStrLn $ T.intercalate "\t" (loss:guess:gold:rest)
  where loss = T.take maxLossLen $ T.pack $ printf "%f" m


withLoss :: Metric -> (Text, Text, [Text]) -> (Double, Text, Text, [Text])
withLoss metric (guess, gold, rest) = ( intRatioToDouble loss
                                      , guess
                                      , gold
                                      , rest )
  where
    loss = getMetric metric guess gold
    intRatioToDouble r = fromIntegral n / fromIntegral d
      where
        d = denominator r
        n = numerator r

getMetric :: Metric -> Text -> Text -> Ratio Int
getMetric CER = M.levenshteinNorm
getMetric WER = werNorm


-- Use edit-distance package to get levenshteinNorm on words instead
-- of characters
werNorm :: Text -> Text -> Ratio Int
werNorm = norm wer

wer :: Text -> Text -> (Int, Int)
wer a b = ( getSum $ fst $ V.leastChanges strCosts va vb
          , max la lb )
  where
    va = V.fromList $ T.words a
    vb = V.fromList $ T.words b
    la = fromIntegral $ V.length va
    lb = fromIntegral $ V.length vb


-- Copied from edit-distance
strCosts :: V.Params Text (String, Int, Text) (Sum Int)
strCosts = V.Params
    { equivalent = (==)
    , delete     = \i c    -> ("delete", i, c)
    , insert     = \i c    -> ("insert", i, c)
    , substitute = \i _ c' -> ("replace", i, c')
    , cost = const (Sum 1)
    , positionOffset = \ (op, _, _) -> if op == "delete" then 0 else 1
    }

-- Copied from text-metrics
norm :: (Text -> Text -> (Int, Int)) -> Text -> Text -> Ratio Int
norm f a b =
  let (r, l) = f a b
   in if r == 0
        then 1 % 1
        else 1 % 1 - r % l
{-# INLINE norm #-}
