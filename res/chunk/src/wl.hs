{-# LANGUAGE BangPatterns #-}
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Word (Word8)
import System.Environment (getArgs)
import System.Exit (die)

-- | Prints the number of bytes on each line of a given text file where
-- escaped characters count as one byte, not two
main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      content <- B.readFile filePath
      mapM_ (go 0 . B.unpack) (BC.lines content)
        where
          go :: Int -> [Word8] -> IO ()
          go !n [] = print n
          go !n (b:bs)
            | b == 92 = case bs of [] -> error "Line terminating with \\"
                                   (_:bs') -> go (n+1) bs'
            | otherwise = go (n+1) bs

    _else -> die "Usage: wl <filepath>"
