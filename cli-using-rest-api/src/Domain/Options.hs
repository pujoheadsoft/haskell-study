{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Domain.Options (Options (..), parseOptions) where

import Options.Applicative
  ( Parser,
    ParserInfo,
    execParser,
    fullDesc,
    header,
    help,
    helper,
    info,
    long,
    metavar,
    progDesc,
    short,
    strOption,
    (<**>),
  )
import Optics.TH (makeFieldLabels)

data Options = Options
  { userId :: String,
    outputPath :: FilePath
  }
makeFieldLabels ''Options

optionUserId :: Parser String
optionUserId =
  strOption
    ( long "userId"
        <> short 'u'
        <> metavar "USER ID"
        <> help "User ID"
    )

optionOutputPath :: Parser FilePath
optionOutputPath =
  strOption
    ( long "output"
        <> short 'o'
        <> metavar "OUTPUT PATH"
        <> help "Output file path"
    )

options :: Parser Options
options =
  Options
    <$> optionUserId
    <*> optionOutputPath

parserInfo :: ParserInfo Options
parserInfo =
  info
    (options <**> helper)
    ( fullDesc
        <> progDesc
          "指定されたユーザーIDをもつユーザーのコメントを取得して指定されたパスのファイルに保存します。\n\
          \ユーザーIDなどは https://jsonplaceholder.typicode.com/ を参照してください。"
        <> header "RestAPIを使うCLIのサンプル"
    )

parseOptions :: IO Options
parseOptions = execParser parserInfo