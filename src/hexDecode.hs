import Numeric (readHex)
import Data.List.Split (chunksOf)
import System.Environment (getArgs)

hexDecode :: String -> String
hexDecode = map (toEnum . fst . head . readHex) . chunksOf 2

main :: IO ()
main = do
    x:_ <- getArgs
    print $ hexDecode x
