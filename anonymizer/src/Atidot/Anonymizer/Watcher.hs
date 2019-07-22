{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Atidot.Anonymizer.Watcher where
import "base"         Control.Monad (forever, void)
import "base"         Control.Concurrent (threadDelay)
import "extra"        Control.Monad.Extra (whenM)
import "bytestring"   Data.ByteString.Char8 (pack, unpack)
import "hinotify"     System.INotify
import                Atidot.Anonymizer.Utils


watcher :: FilePath
        -- ^ Directory to watch
        -> (FilePath -> IO ())
        -- ^ Handler
        -> IO ()
watcher directory handler = withINotify $ \inotify -> do
    _ <- addWatch
        inotify
        [MoveIn, CloseWrite]
        (pack directory)
        internalHandler
    void $ forever $ threadDelay 5000000

    where
        internalHandler (Closed False (Just path') True) = do
            whenM (isXML path)  $ handler path
            whenM (isCSV path)  $ handler path
            whenM (isJSON path) $ handler path
            return ()
            where
                path = unpack path'

        internalHandler (MovedIn False path' _) = do
            whenM (isXML path)  $ handler path
            whenM (isCSV path)  $ handler path
            whenM (isJSON path) $ handler path
            return ()
            where
                path = unpack path'

        internalHandler _ = return ()
