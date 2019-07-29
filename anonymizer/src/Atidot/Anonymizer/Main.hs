{-# LANGUAGE PackageImports #-}
module Atidot.Anonymizer.Main where

import "base"                 Data.Semigroup ((<>))
--import "base"                 Data.Maybe
import "filepath"             System.FilePath
import "directory"            System.Directory
import "text"                 Data.Text (Text)
import "optparse-applicative" Options.Applicative
import                        Atidot.Anonymizer.Monad
import                        Atidot.Anonymizer.Load (load)
import                        Atidot.Anonymizer.Watcher (watcher)
import                        Atidot.Anonymizer.Run
import                        Atidot.Anonymizer.Server (runServer)
import                        Atidot.Anonymizer.Swagger (printSwagger)
import qualified "bytestring" Data.ByteString.Lazy as BL
import qualified "bytestring" Data.ByteString.Lazy.Char8 as BL8
import "extra"                Control.Monad.Extra
import "base"                 Data.List (isSuffixOf)
import Debug.Trace

lttrace :: Show a => [Char] -> a -> a
lttrace x y = trace (x ++ ":" ++ show y) y

data Listener = Listener
    { _listener_key    :: !Text
    , _listener_input  :: !FilePath
    , _listener_output :: !FilePath
    , _listener_script :: !(Maybe FilePath)
    }

data RunScript = RunScript
    { _runScript_key    :: !Text
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
    { _apiServer_key    :: !Text
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

runScriptParser :: Parser Command
runScriptParser = C1 <$> (RunScript
    <$> strOption
      ( long "key"
     <> short 'k'
     <> metavar "KEY"
     <> help "Hashing Key"
      )
    <*> strOption
      ( long "input"
     <> short 'i'
     <> metavar  "PATH"
     <> help "Input File"
      )
    <*> optional (strOption
      ( long "output"
     <> short 'o'
     <> metavar  "PATH"
     <> help "Output Directory"
      ))
    <*> optional (strOption
      ( long "script"
     <> short 's'
     <> metavar "PATH"
     <> help "Anonymizer Script to use"
      ))
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
    <$> strOption
      ( long "key"
     <> short 'k'
     <> metavar "KEY"
     <> help "Hashing Key"
      )
    <*> strOption
      ( long "input"
     <> short 'i'
     <> metavar  "PATH"
     <> help "Input Directory"
      )
    <*> strOption
      ( long "output"
     <> short 'o'
     <> metavar  "PATH"
     <> help "Output Directory"
      )
    <*> optional (strOption
      ( long "script"
     <> short 's'
     <> metavar "PATH"
     <> help "Anonymizer Script to use"
      ))
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
listen config@(Listener _ inputDir outDir scriptFP) = do
    unlessM (doesDirectoryExist inputDir) $ error $
        "Annonymizer: Expected directory as input, got: " ++ show inputDir
    unlessM (doesDirectoryExist outDir) $ error $
        "Annonymizer: Expected directory as output, got: " ++ show outDir
    script <- loadScript scriptFP
    watcher
        inputDir
        (listenInternal script config)
    where
        listenInternal script _config filepath =
          runSingleFile script filepath $ Just outDir

getPaths :: GetPaths -> IO ()
getPaths (GetPaths inputFile) =
    print =<< run getAllPaths inputFile

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
runScript (RunScript _ filepath mOutDir scriptFp) = do
    _ <- isValidInput filepath
    _ <- isOutputDir mOutDir
    script <- loadScript scriptFp
    runSingleFile script filepath mOutDir

runSingleFile :: Anonymizer Text -> FilePath -> Maybe FilePath -> IO ()
runSingleFile script filepath mOutfile = do
    (_,res) <- run script filepath
    case mOutfile of
        Just outDir -> do -- TODO: this can be maybe fmapped
            let fExt = takeExtension filepath
                fName = takeBaseName filepath
                outFile = outDir </> fName <.> "anon" <.> fExt -- TODO: add date as numerical value
            BL.writeFile outFile res
        Nothing -> BL8.putStrLn res
    return () -- TODO: ask berko about return values

loadScript :: Maybe FilePath -> IO (Anonymizer Text)
loadScript = maybe (return testScript) load

isValidFormat :: FilePath -> Bool
isValidFormat fp = (`isSuffixOf` fp) `any` validFormats


validFormats :: [String]
validFormats = ["xml","json","csv"]

isValidInput :: FilePath -> IO ()
isValidInput fp = do
    let valid = isValidFormat fp
    exists <- doesFileExist fp
    unless (exists && valid) $ error $ unlines $
        [ "Annonymizer: Input File Error, got: " ++ show fp
        , "Valid formats are: " ++ show validFormats
        ]
    return ()

isOutputDir :: Maybe FilePath -> IO ()
isOutputDir (Just outDir)= do
    exists <- doesDirectoryExist outDir
    unless exists $ error $ unlines $
        [ "Annonymizer: Expected directory as output, got: " ++ show outDir
        , "Valid formats are: " ++ show validFormats
        ]
isOutputDir _ = return ()