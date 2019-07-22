{-# LANGUAGE PackageImports #-}
module Atidot.Anonymizer.Main where

import "base"                 Data.Semigroup ((<>))
import "text"                 Data.Text (Text)
import "optparse-applicative" Options.Applicative
import                        Atidot.Anonymizer.Monad
import                        Atidot.Anonymizer.Load (load)
import                        Atidot.Anonymizer.Watcher (watcher)
import                        Atidot.Anonymizer.Run
import                        Atidot.Anonymizer.Server (runServer)
import                        Atidot.Anonymizer.Swagger (printSwagger)


data Listener
    = Listener
    { _listener_key    :: !Text
    , _listener_input  :: !FilePath
    , _listener_output :: !FilePath
    , _listener_watch  :: !Bool
    , _listener_script :: !(Maybe FilePath)
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
    = C1 Listener
    | C2 GetPaths
    | C3 APIServer
    | C4 Swagger

listenerParser :: Parser Command
listenerParser = C1 <$> (Listener
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
    <*> switch
      ( long "listen"
     <> short 'l'
     <> help "Listen to Input Directory changes"
      )
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


combined :: Parser Command
combined = subparser
         ( command "listener" (info listenerParser (progDesc "Listen to directory changes"))
        <> command "paths" (info getPathsParser (progDesc "Print all paths"))
        <> command "api"   (info apiServerParser (progDesc "Run API server"))
        <> command "swagger" (info swaggerParser (progDesc "Print Swagger Specification"))
         )

listen :: Listener -> IO ()
listen config = do
    script <- case _listener_script config of
        Nothing -> return testScript
        Just path -> load path
    if _listener_watch config
    then watcher
        (_listener_input config)
        (listenInternal script config)
    else do
        -- TODO: change to run on all files in directory?
        result <- run script (_listener_input config)
        print result
    return ()
    where
        listenInternal script _config filepath = do
            _ <- run script filepath
            return ()

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
        C1 config -> listen config
        C2 config -> getPaths config
        C3 config -> apiServer config
        C4 config -> swagger config
