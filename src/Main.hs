module Main where

import Data.Aeson(encode, ToJSON(toJSON), object, (.=))
import qualified Data.Text as T
import Data.Char (toUpper)
import Control.Monad (mzero)
import System.IO (hGetLine, hIsEOF, hClose, utf8, hSetEncoding, openFile, IOMode(ReadMode), Handle)
import Data.Text.Encoding (decodeUtf8)

newtype Company = Company
  { csv :: T.Text
  } deriving Show

instance ToJSON Company where
    toJSON (Company csv) =
      object [ T.pack "csv" .= csv ]

main :: IO ()
main = do
  inh <- openFile "/home/notation/Downloads/skatteliste-2019.csv" ReadMode
  hSetEncoding inh utf8
  mainLoop inh
  putStrLn "Finished"
  hClose inh

splitCSV :: T.Text -> [T.Text]
splitCSV =
  T.splitOn $ T.pack ","

mainLoop :: Handle -> IO ()
mainLoop inh = do
  ineof <- hIsEOF inh
  if ineof
    then return ()
    else do
    inpStr <- hGetLine inh
    let company_json = encode . Company . head . splitCSV . T.pack $ inpStr
    print $ show company_json
    mainLoop inh
