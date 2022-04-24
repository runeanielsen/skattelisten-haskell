module Main where

import qualified Basement.Types.CharUTF8 as B
import Control.Monad (mzero)
import Data.Aeson (ToJSON (toJSON), encode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char (toUpper)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import RIO.ByteString.Lazy (unpack)
import qualified RIO.ByteString.Lazy as B.CharUTF8
import qualified RIO.ByteString.Lazy as Data.Aeson
import System.IO
  ( Handle,
    IOMode (ReadMode, WriteMode),
    hClose,
    hGetLine,
    hIsEOF,
    hPrint,
    hPutStrLn,
    hSetEncoding,
    openFile,
    utf8,
  )

data Company = Company
  { csv :: T.Text,
    name :: T.Text,
    se :: T.Text,
    income_year :: T.Text,
    company_type :: T.Text,
    taxable_income :: T.Text,
    deficit :: T.Text,
    corporate_tax :: T.Text
  }
  deriving (Show)

instance ToJSON Company where
  toJSON (Company csv name se income_year company_type taxable_income deficit corporate_tax) =
    object
      [ T.pack "csv" .= csv,
        T.pack "name" .= name,
        T.pack "se" .= se,
        T.pack "income_year" .= income_year,
        T.pack "company_type" .= company_type,
        T.pack "taxable_income" .= taxable_income,
        T.pack "deficit" .= deficit,
        T.pack "corporate_tax" .= corporate_tax
      ]

main :: IO ()
main = do
  inh <- openFile "" ReadMode
  ouh <- openFile "" WriteMode
  hSetEncoding inh utf8
  _ <- hGetLine inh -- Skip first line
  mainLoop inh ouh
  hClose inh

createCompany :: [T.Text] -> Company
createCompany line =
  Company
    (head line)
    (line !! 1)
    (line !! 2)
    (line !! 3)
    (line !! 5)
    (line !! 8)
    (line !! 9)
    (line !! 10)

splitCSV :: T.Text -> [T.Text]
splitCSV =
  T.splitOn $ T.pack ","

mainLoop :: Handle -> Handle -> IO ()
mainLoop inh ouh = do
  ineof <- hIsEOF inh
  if ineof
    then return ()
    else do
      inpStr <- hGetLine inh
      let company_json = encode . createCompany . splitCSV . T.pack $ inpStr
      hPutStrLn ouh $ B.unpack company_json
      mainLoop inh ouh
