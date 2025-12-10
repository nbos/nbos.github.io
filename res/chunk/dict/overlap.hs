import qualified Data.Set as Set
import System.Environment (getArgs)

main :: IO ()
main = do
    [file1, file2] <- getArgs
    content1 <- readFile file1
    content2 <- readFile file2
    
    let sets1 = tail $ scanl (flip Set.insert) Set.empty (lines content1)
        sets2 = tail $ scanl (flip Set.insert) Set.empty (lines content2)
        similarities = zipWith similarity sets1 sets2
        csv = zipWith (\n r -> show n ++ "," ++ show r) [256..] similarities
    
    putStr (unlines csv)

similarity :: Set.Set String -> Set.Set String -> Double
similarity s1 s2 = i / n
  where
    i = fromIntegral $ Set.size (s1 `Set.intersection` s2)
    n = fromIntegral $ Set.size s1
