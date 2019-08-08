{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Atidot.Anonymizer.CSV where

import           "base"                 Control.Monad.IO.Class (MonadIO)
import           "base"                 Control.Monad
import           "lens"                 Control.Lens
import           "exceptions"           Control.Monad.Catch (MonadMask, bracket)
import           "mtl"                  Control.Monad.State.Class (MonadState, gets)
import           "free"                 Control.Monad.Free (iterM)
import           "data-default"         Data.Default
import qualified "bytestring"           Data.ByteString.Lazy as BL
import qualified "bytestring"           Data.ByteString.Char8 as B
import qualified "unordered-containers" Data.HashMap.Lazy as HM
import           "base"                 Data.List (sort, nub, transpose)
import           "text"                 Data.Text (Text, pack, unpack)
import qualified "vector"               Data.Vector as V (toList, fromList)
import           "cassava"              Data.Csv hiding ((.=))
import           "ListZipper"           Data.List.Zipper
import                                  Atidot.Anonymizer.Monad
import                                  Atidot.Anonymizer.Types
import                                  Atidot.Anonymizer.Utils (anonymize)
import           "base"                 Control.Arrow hiding (right)

parseBoth :: (FromField a, FromField b) => (Field, Field) -> Parser (a, b)
parseBoth (k, v) = (,) <$> parseField k <*> parseField v


type ColumnName = Text
type CellValue = Text
type NamedTextRecord = [(ColumnName, CellValue)]
instance (FromField a, FromField b, Ord a) => FromNamedRecord [(a,b)] where
    parseNamedRecord m = traverse parseBoth $ HM.toList m

type CSV = Zipper NamedTextRecord
type Cursor = (Int, Int)

--TODO: header, current, can be removed
--TODO: ensure empty case is supported
data CSVState
    = CSVState
    { _csvState_header      :: ![Text]
    , _csvState_csvZipper   :: !CSV
    , _csvState_current     :: !Cursor
    , _csvState_paths       :: ![Path]
    , _csvState_hashed      :: ![Text]
    , _csvState_encrypted   :: ![Text]
    , _csvState_whitelisted :: ![Text]
    , _csvState_blacklisted :: ![Text]
    , _csvState_hashKey     :: !Text
    } deriving (Show)

instance Default CSVState where
    def = CSVState []
                   empty
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
                Right (header', vss) -> do
                    let header''     = map (pack . B.unpack) . V.toList $ header'
                        cols         = fromList $ transpose $ V.toList vss
                    csvState_header       .= header''
                    csvState_csvZipper    .= cols
                    csvState_paths  .= map (\x -> [x]) header''
                    unless (beginp cols) $ error "csv zipper is not at the beginning"
                Left err -> error err
            return ()

        fini _ = return ()

        body _ = do
            res <- iterM run action
            _modifiedCsv <- toList <$> gets _csvState_csvZipper
            headers <- gets _csvState_header
            let _columnValues = undefined
                _headers' = V.fromList $ map (B.pack . unpack) headers
                outBS = "" -- encodeByName headers' columnValues
            return (res, outBS)

        run :: (MonadState CSVState m, MonadIO m, MonadMask m) => AnonymizerCmd (m a) -> m a
        run (Current return') = do

            cursor_ <- cursor <$> gets _csvState_csvZipper
            let columnName = getColumnName cursor_
                path = [columnName]
            return' path

        run (Next return') = do
            cursor_                    <- gets _csvState_csvZipper
            let next_ = right cursor_
            if endp next_
            then return' False
            else do
                csvState_csvZipper .= next_
                return' True
        run (Hash next') = do
            zipper_ <-  gets _csvState_csvZipper
            k <- gets _csvState_hashKey
            let cursor_ = cursor zipper_
                anonymize' :: Text -> Text
                anonymize' = pack . anonymize k . unpack
                anonymizedCursor = map (second anonymize' ) cursor_
            csvState_csvZipper .= replace anonymizedCursor zipper_
            csvState_hashed <>= [getColumnName cursor_]
            next'

        run (Encrypt _next') = anonymizerError "Encryption is not yet supported for CSV" --do
        --     cursor <- gets _csvState_current
        --     csvState_encrypted <>= [cursor]
        --     next'

        run (Paths return') = do
            paths' <- gets _csvState_paths
            return' paths'

        run (ChangedPaths return') = do
            hashed    <- gets _csvState_hashed
            encrypted <- gets _csvState_encrypted
            let changedPaths'
                    = map (:[])
                    . nub
                    . sort
                    $ (hashed <> encrypted)

            return' changedPaths'

getColumnName :: [(c, b)] -> c
getColumnName = fst . head