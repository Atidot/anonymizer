{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
module Atidot.Anonymizer.Main where

import "base"                 Data.Semigroup ((<>))
import "base"                 Data.List (isSuffixOf)
import "directory"            System.Directory
import "text"                 Data.Text (Text)
import "optparse-applicative" Options.Applicative
import "extra"                Control.Monad.Extra

import                        Atidot.Anonymizer.Monad
import                        Atidot.Anonymizer.Load (loadScript)
import                        Atidot.Anonymizer.Watcher (watcher)
import                        Atidot.Anonymizer.Run
import                        Atidot.Anonymizer.Server (runServer)
import                        Atidot.Anonymizer.Swagger (printSwagger)

import qualified "text"       Data.Text.IO as T

data Listener = Listener
    { _listener_key    :: !FilePath
    , _listener_input  :: !FilePath
    , _listener_output :: !FilePath
    , _listener_script :: !(Maybe FilePath)
    }

data RunScript = RunScript
    { _runScript_key    :: !FilePath
    , _runScript_input  :: !FilePath
    , _runScript_output :: !(Maybe FilePath)
    , _runScript_script :: !(Maybe FilePath)
    }

data GetPaths
    = GetPaths
    { _getPaths_input :: !FilePath
    }

data APIServer
    = APIServer
    { _apiServer_key    :: !FilePath
    , _apiServer_port   :: !Int
    , _apiServer_sslCrt :: !FilePath
    , _apiServer_sslKey :: !FilePath
    }

data Swagger
    = Swagger
    { _swagger_print :: !Bool
    }


data Command
    = C1 RunScript
    | C2 GetPaths
    | C3 APIServer
    | C4 Swagger
    | C5 Listener

hashingKeyOpt :: Parser FilePath
hashingKeyOpt = strOption
    ( long "key"
    <> short 'k'
    <> metavar "KEY"
    <> help "Hashing Key"
    )

outDirOpt :: Parser FilePath
outDirOpt = strOption
    ( long "output"
    <> short 'o'
    <> metavar  "PATH"
    <> help "Output Directory"
    )

scriptOpt :: Parser FilePath
scriptOpt = strOption
    ( long "script"
    <> short 's'
    <> metavar "PATH"
    <> help "Anonymizer Script to use"
    )

runScriptParser :: Parser Command
runScriptParser = C1 <$> (RunScript
    <$> hashingKeyOpt
    <*> strOption
      ( long "input"
     <> short 'i'
     <> metavar  "PATH"
     <> help "Input File"
      )
    <*> optional outDirOpt
    <*> optional scriptOpt
    )

getPathsParser :: Parser Command
getPathsParser = C2 <$> (GetPaths
    <$> strOption
      ( long "input"
     <> short 'i'
     <> metavar "PATH"
     <> help "Input Filename"
      ))

apiServerParser :: Parser Command
apiServerParser = C3 <$> (APIServer
    <$> strOption
      ( long "key"
     <> short 'k'
     <> metavar "KEY"
     <> help "Hashing Key"
      )
    <*> (read <$> (strOption
      ( long "port"
     <> short 'p'
     <> metavar "PORT"
     <> help "Port number"
      )))
    <*> strOption
      ( long "sslCrt"
     <> metavar "PATH"
     <> help "SSL Cert File"
      )
    <*> strOption
      ( long "sslKey"
     <> metavar "PATH"
     <> help "SSL Key File"
      )
    )

swaggerParser :: Parser Command
swaggerParser = C4 <$> (Swagger
    <$> switch
      ( long "print"
     <> short 'p'
      )
    )

listenerParser :: Parser Command
listenerParser = C5 <$> (Listener
    <$> hashingKeyOpt
    <*> strOption
      ( long "input"
     <> short 'i'
     <> metavar  "PATH"
     <> help "Input Directory"
      )
    <*> outDirOpt
    <*> optional scriptOpt
    )

combined :: Parser Command
combined = subparser
         ( command "run" (info runScriptParser (progDesc "Runs Script"))
        <> command "paths" (info getPathsParser (progDesc "Print all paths"))
        <> command "api"   (info apiServerParser (progDesc "Run API server"))
        <> command "swagger" (info swaggerParser (progDesc "Print Swagger Specification"))
        <> command "listener" (info listenerParser (progDesc "Listen to directory changes"))
         )

listen :: Listener -> IO ()
listen (Listener hashKeyFp inputDir outDir scriptFP) = do
    hashKey <- verifyHashingKey hashKeyFp
    unlessM (doesDirectoryExist inputDir) $ error $
        "Annonymizer: Expected directory as input, got: " ++ show inputDir
    unlessM (doesDirectoryExist outDir) $ error $
        "Annonymizer: Expected directory as output, got: " ++ show outDir
    script <- loadScript scriptFP
    watcher
        inputDir
        (listenInternal hashKey script)
    where
        listenInternal hashKey script filepath = do
            _ <- runSingleFile hashKey filepath script $ Just outDir
            return ()

getPaths :: GetPaths -> IO ()
getPaths (GetPaths inputFile) =
    print =<< run "" inputFile getAllPaths

apiServer :: APIServer -> IO ()
apiServer (APIServer _key port sslCrt sslKey) = do
    runServer port sslCrt sslKey
    return ()

swagger :: Swagger -> IO ()
swagger _ = do
    printSwagger
    return ()

main :: IO ()
main = do
    options <- execParser (info (combined <**> helper) (fullDesc <> progDesc "Atidot Anonymizer"))
    case options of
        C1 config -> runScript config
        C2 config -> getPaths config
        C3 config -> apiServer config
        C4 config -> swagger config
        C5 config -> listen config

runScript :: RunScript -> IO ()
runScript (RunScript hashKeyFp filepath mOutDir scriptFp) = do
    hashKey <- verifyHashingKey hashKeyFp
    verifyInputFile filepath
    verifyOutputDir mOutDir
    script <- loadScript scriptFp
    _ <- runSingleFile hashKey filepath script mOutDir
    return ()




isValidFormat :: FilePath -> Bool
isValidFormat fp = (`isSuffixOf` fp) `any` validFormats


validFormats :: [String]
validFormats = ["xml","json","csv"]

verifyInputFile :: FilePath -> IO ()
verifyInputFile fp = do
    exists <- doesFileExist fp
    unless exists $ error $ "No such file: " ++ fp
    unless (isValidFormat fp) $ error $ unlines
        [ "Input File Error, got: " ++ show fp
        , "Valid formats are: " ++ show validFormats
        ]

verifyOutputDir :: Maybe FilePath -> IO ()
verifyOutputDir (Just outDir)= do
    exists <- doesDirectoryExist outDir
    unless exists $ error $ unlines
        [ "Annonymizer: Expected directory as output, got: " ++ show outDir
        , "Valid formats are: " ++ show validFormats
        ]
verifyOutputDir _ = return ()

verifyHashingKey :: FilePath -> IO Text
verifyHashingKey fp = do
    exists <- doesFileExist fp
    unless exists $ error $
        "Annonymizer: Hashing key Error, is not a file, got: " ++ show fp
    T.readFile fp