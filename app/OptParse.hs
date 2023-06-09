module OptParse
  ( Options(..)
  , SingleInput(..)
  , SingleOutput(..)
  , parse
  )
  where

import HsBlog.Env

import Data.Maybe (fromMaybe)
import Options.Applicative

-- | Model
data Options
  = ConvertSingle SingleInput SingleOutput
  | ConvertDir FilePath FilePath Env
  deriving Show

data SingleInput
  = Stdin
  | InputFile FilePath
  deriving Show

data SingleOutput
  = Stdout
  | OutputFile FilePath
  deriving Show

-- Parser
parse :: IO Options
parse =
  execParser opts

opts :: ParserInfo Options
opts =
  info (pOptions <**> helper)
    ( fullDesc
      <> header "hs-blog-gen - a static blog generator"
      <> progDesc "Convert markup files or directories to html"
    )

pOptions :: Parser Options
pOptions =
  subparser
    ( command
      "convert"
      ( info
        (helper <*> pConvertSingle)
        (progDesc "Convert a single markup source to html")
      )
      <> command
      "convert-dir"
      ( info
        (helper <*> pConvertDir)
        (progDesc "Convert a directory of markup files to html")
      )
    )

pConvertSingle :: Parser Options
pConvertSingle =
  ConvertSingle <$> pSingleInput <*> pSingleOutput

pSingleInput :: Parser SingleInput
pSingleInput =
  fromMaybe Stdin <$> optional pInputFile

pSingleOutput :: Parser SingleOutput
pSingleOutput =
  fromMaybe Stdout <$> optional pOutputFile

pInputFile :: Parser SingleInput
pInputFile = fmap InputFile parser
  where
    parser =
      strOption
        ( long "input"
          <> short 'i'
          <> metavar "FILE"
          <> help "Input file"
        )

pOutputFile :: Parser SingleOutput
pOutputFile = OutputFile <$> parser -- fmap and <$> are the same
  where
    parser =
      strOption
        ( long "output"
          <> short 'o'
          <> metavar "FILE"
          <> help "Output file"
        )

-- * Directory conversion parser
pConvertDir :: Parser Options
pConvertDir =
  ConvertDir <$> pInputDir <*> pOutputDir <*> pEnv

pInputDir :: Parser FilePath
pInputDir =
  strOption
    ( long "input"
      <> short 'i'
      <> metavar "FILE"
      <> help "Input file"
    )

pOutputDir :: Parser FilePath
pOutputDir =
  strOption
    ( long "output"
      <> short 'o'
      <> metavar "FILE"
      <> help "Output file"
    )

pEnv :: Parser Env
pEnv =
  Env <$> pBlogName <*> pStylesheet

pBlogName :: Parser String
pBlogName =
  strOption
    ( long "name"
      <> short 'N'
      <> metavar "STRING"
      <> help "Blog name"
      <> value (eBlogName defaultEnv)
      <> showDefault
    )

pStylesheet :: Parser String
pStylesheet =
  strOption
    ( long "style"
      <> short 'S'
      <> metavar "FILE"
      <> help "Stylesheet filename"
      <> value (eStylesheetPath defaultEnv)
      <> showDefault
    )
