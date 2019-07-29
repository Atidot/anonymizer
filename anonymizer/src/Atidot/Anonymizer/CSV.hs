{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Atidot.Anonymizer.CSV where

import           "base"                 Control.Monad.IO.Class (MonadIO)
import           "lens"                 Control.Lens
import           "exceptions"           Control.Monad.Catch (MonadMask, bracket)
import           "mtl"                  Control.Monad.State.Class (MonadState, gets)
import           "free"                 Control.Monad.Free (iterM)
import           "data-default"         Data.Default
import qualified "bytestring"           Data.ByteString.Lazy as BL
import qualified "bytestring"           Data.ByteString.Char8 as B
import qualified "unordered-containers" Data.HashMap.Lazy as HM
import           "base"                 Data.List (sort, nub)
import           "text"                 Data.Text (Text, pack)
import           "vector"               Data.Vector (Vector)
import qualified "vector"               Data.Vector as V (length, fromList, toList)
import           "cassava"              Data.Csv hiding ((.=))
import                                  Atidot.Anonymizer.Monad
import                                  Atidot.Anonymizer.Types


parseBoth :: (FromField a, FromField b) => (Field, Field) -> Parser (a, b)
parseBoth (k, v) = (,) <$> parseField k <*> parseField v

type NamedTextRecord = [(Text, Text)]
instance (FromField a, FromField b, Ord a) => FromNamedRecord [(a,b)] where
    parseNamedRecord m = (traverse parseBoth $ HM.toList m)

type CSV = Vector NamedTextRecord
type Cursor = (Int, Int)
data CSVState
    = CSVState
    { _csvState_header      :: ![Text]
    , _csvState_csv         :: !CSV
    , _csvState_current     :: !Cursor
    , _csvState_paths       :: ![Path]
    , _csvState_hashed      :: ![Cursor]
    , _csvState_encrypted   :: ![Cursor]
    , _csvState_whitelisted :: ![Cursor]
    , _csvState_blacklisted :: ![Cursor]
    , _csvState_hashKey     :: !Text
    } deriving (Show)

instance Default CSVState where
    def = CSVState []
                   (V.fromList [])
                   (0,0)
                   []
                   []
                   []
                   []
                   []
                   ""

makeLenses ''CSVState

runCSV :: (MonadState CSVState m, MonadIO m, MonadMask m)
       => Anonymizer a
       -> BL.ByteString
       -> m (a, BL.ByteString)
runCSV action csvData
    = bracket init'
              fini
              body
    where
        init' = do
            case decodeByName csvData of
                Left err -> error err
                Right (header', vss) -> do
                    let header''     = (map (pack . B.unpack) . V.toList $ header')
                    csvState_header .= header''
                    csvState_csv    .= vss
                    csvState_paths  .= map (\x -> [x]) header''
                    return ()
            return ()

        fini _ = return ()

        body _ = do
            res <- iterM run action
            return (res, "")

        run :: (MonadState CSVState m, MonadIO m, MonadMask m) => AnonymizerCmd (m a) -> m a
        run (Current return') = do
            header'          <- gets _csvState_header
            (_, columnIndex) <- gets _csvState_current
            let column = header' !! columnIndex
            let path = [column]
            return' path

        run (Next return') = do
            header'                 <- gets _csvState_header
            (rowIndex, columnIndex) <- gets _csvState_current
            if columnIndex < length header' - 1
            then do
                let newColumnIndex = columnIndex + 1
                csvState_current .= (rowIndex, newColumnIndex)
                return' True
            else do
                csv <- gets _csvState_csv
                if rowIndex >= V.length csv - 1
                then return' False
                else do
                    let newRowIndex = rowIndex + 1
                    let newColumnIndex = 0
                    csvState_current .= (newRowIndex, newColumnIndex)
                    return' True


        run (Hash next') = do
            cursor <- gets _csvState_current
            --hashKey <- gets _csvState_hashKey
            csvState_hashed <>= [cursor]
            --anonymize hashKey cursor
            next'

        run (Encrypt next') = do
            cursor <- gets _csvState_current
            csvState_encrypted <>= [cursor]
            next'

        run (Paths return') = do
            paths' <- gets _csvState_paths
            return' paths'

        run (ChangedPaths return') = do
            header'   <- gets _csvState_header
            hashed    <- gets _csvState_hashed
            encrypted <- gets _csvState_encrypted
            let changedPaths'
                    = map (\index' -> [header' !! index'])
                    . nub
                    . sort
                    . snd
                    $ (unzip hashed <> unzip encrypted)

            return' changedPaths'
