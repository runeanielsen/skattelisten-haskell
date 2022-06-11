module Main where

import Data.Aeson (ToJSON (toJSON), encode, object, (.=))
import qualified Data.ByteString.Lazy.UTF8 as B
import Data.Char (toUpper)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import System.Environment (getArgs)
import System.IO
  ( Handle,
    IOMode (ReadMode, WriteMode),
    hClose,
    hFlush,
    hGetLine,
    hIsEOF,
    hPrint,
    hPutStrLn,
    hSetEncoding,
    openFile,
    utf8,
  )

data CmdArgs = CmdArgs
  { input_file :: String,
    output_file :: String
  }
  deriving (Show)

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

createCompanyParsedCsv :: [T.Text] -> Company
createCompanyParsedCsv line =
  Company
    (head line)
    (line !! 1)
    (line !! 2)
    (line !! 3)
    (line !! 5)
    (line !! 8)
    (line !! 9)
    (line !! 10)

splitCsv :: T.Text -> [T.Text]
splitCsv =
  T.splitOn $ T.pack ","

csvLineToCompany :: T.Text -> Company
csvLineToCompany =
  createCompanyParsedCsv . splitCsv

processCsvBody :: ToJSON a => (T.Text -> a) -> Handle -> Handle -> IO ()
processCsvBody fn inh ouh = do
  ineof <- hIsEOF inh
  if ineof
    then return ()
    else do
      inpStr <- hGetLine inh
      hPutStrLn ouh (B.toString . encode . fn . T.pack $ inpStr)
      processCsvBody fn inh ouh

processSkattelisten :: String -> String -> IO ()
processSkattelisten input_file output_file = do
  inh <- openFile input_file ReadMode
  ouh <- openFile output_file WriteMode
  hSetEncoding inh utf8
  _ <- hGetLine inh -- Skip CSV header
  processCsvBody csvLineToCompany inh ouh
  hFlush ouh
  hClose inh
  hClose ouh

parseArgs :: [String] -> CmdArgs
parseArgs args =
  CmdArgs (head args) (args !! 1)

main :: IO ()
main = do
  args <- getArgs
  let cmdArgs = parseArgs args
  processSkattelisten (input_file cmdArgs) (output_file cmdArgs)
