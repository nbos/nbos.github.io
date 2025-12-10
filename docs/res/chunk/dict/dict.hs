import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Word (Word8)
import System.Environment (getArgs)
import System.Exit (die)

-- | \\
esc :: Word8
esc = 92

-- | ,
comma :: Word8
comma = 44

-- | ' '
space :: Word8
space = 32

-- | Our CSVs fields have a leading space
trimSpace :: [Word8] -> [Word8]
trimSpace (b:bs) | b == space = bs
trimSpace bs = bs

-- | Prints the string in the last field of a CSV that possibly contains
-- escaped commas
main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      content <- B.readFile filePath
      mapM_ (go [10] . reverse . B.unpack) (BC.lines content)
        where
          go :: [Word8] -> [Word8] -> IO ()
          go _ [] = error "Reached begining of line looking for comma"
          go acc (b:bs)
            | b == comma = case bs of
                [] -> B.putStr $ B.pack $ trimSpace acc -- weird but w/e
                (b':bs') | b' == esc -> go (esc:comma:acc) bs'
                         | otherwise -> B.putStr $ B.pack $ trimSpace acc -- end
            | otherwise = go (b:acc) bs

    _else -> die "Usage: dict <filepath>"
