{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module Atidot.Anonymizer.XML where

import "base"           Control.Monad (void)
import "base"           Control.Monad.IO.Class (MonadIO, liftIO)
import "base"           Control.Applicative ((<|>))
import "lens"           Control.Lens
import "exceptions"     Control.Monad.Catch (MonadMask, bracket)
import "mtl"            Control.Monad.State.Class (MonadState, gets)
import "free"           Control.Monad.Free (iterM)
import "base"           Data.Maybe (maybe, maybeToList)
import "base"           Data.Char (isSpace, toLower)
import "data-default"   Data.Default
import "text"           Data.Text (Text)
import "bloomfilter"    Data.BloomFilter.Easy (Bloom)
import "hxt"            Data.Tree.NTree.Zipper.TypeDefs
import "hxt"            Data.Tree.NTree.TypeDefs
import "hxt"            Text.XML.HXT.Core
import "hxt"            Text.XML.HXT.Arrow.XmlState.RunIOStateArrow
import "hxt"            Text.XML.HXT.Arrow.XmlState.TypeDefs
import "temporary"      System.IO.Temp (emptySystemTempFile)

import                  Atidot.Anonymizer.Monad
import                  Atidot.Anonymizer.Types
import                  Atidot.Anonymizer.Utils (anonymize, removeIfExists)

import qualified "bytestring"   Data.ByteString.Lazy  as BL
import qualified "text"         Data.Text             as T
import qualified "bloomfilter"  Data.BloomFilter.Easy as Bloom

data XMLState
    = XMLState
    { _xmlState_mZipper      :: !(Maybe (NTZipper XNode))
    , _xmlState_attrIndex    :: !Int
    , _xmlState_paths        :: ![Path]
    , _xmlState_fNameBloom   :: Bloom String
    , _xmlState_changedPaths :: ![Path]
    , _xmlState_hashKey      :: !Text
    } deriving (Show)

instance Default XMLState where
    def = XMLState
        Nothing
        0
        []
        (Bloom.easyList 1.0 [])
        []
        ""

makeLenses ''XMLState

runXML :: (MonadState XMLState m, MonadIO m, MonadMask m)
       => Anonymizer a
       -> BL.ByteString
       -> m (a, BL.ByteString)
runXML = runXMLInner True -- can edit

runXMLInner :: (MonadState XMLState m, MonadIO m, MonadMask m)
       => Bool
       -> Anonymizer a
       -> BL.ByteString
       -> m (a, BL.ByteString)
runXMLInner isEdit action xmlData
    = bracket init'
              fini
              body
    where
        init' = do
            xmlPath <- liftIO $ emptySystemTempFile "anonymizer_input.xml"
            liftIO $ BL.writeFile xmlPath xmlData
            zipper <- liftIO $ toNTZipper . head <$> runX (readDocument [] xmlPath)
            xmlState_mZipper .= Just zipper
            liftIO $ removeIfExists xmlPath

        fini _ = return ()

        writeDoc :: (MonadIO m) => FilePath -> NTree XNode -> m ()
        writeDoc fp = void . liftIO . runIOSLA (writeDocument [] fp) (XIOState initialSysState ())

        body _ = do
            res <- iterM (runWrap isEdit xmlData) action
            xmlPath <- liftIO $ emptySystemTempFile "anonymizer_input.xml"
            mDoc <- fmap getRoot <$> gets _xmlState_mZipper
            maybe
                (error "final zipper was Nothing, not supposed to happen")
                (writeDoc xmlPath)
                mDoc
            outBS <- liftIO $ BL.readFile xmlPath
            liftIO $ removeIfExists xmlPath
            return (res, outBS)

runWrap :: (MonadState XMLState m, MonadIO m, MonadMask m)
    => Bool
    -> BL.ByteString
    -> AnonymizerCmd (m a)
    -> m a
runWrap isEdit _xmlData cmd =
    case cmd of
        (Hash _next') | not isEdit -> error "Cannot hash in check"
        _ -> run cmd

run :: (MonadState XMLState m, MonadIO m, MonadMask m)
    => AnonymizerCmd (m a)
    -> m a
run  = \case
    (Current return') -> do
        mZipper <- gets _xmlState_mZipper
        return' $ mGetPath mZipper

    (Next return') -> do
        paths'  <- gets _xmlState_paths
        mZipper <- gets _xmlState_mZipper
        case mNext mZipper of
            Just zipper -> do
                xmlState_paths .= paths' <> [getPath zipper]
                xmlState_mZipper .= Just zipper
                return' True
            Nothing -> return' False

    (Hash next') -> do
        mZipper <- gets _xmlState_mZipper
        hashKey <- gets _xmlState_hashKey
        xmlState_mZipper .= (hashZipper hashKey <$> mZipper)
        addUpdatedPath mZipper
        next'

    (Paths return') ->
        return' =<< gets _xmlState_paths

    (ChangedPaths return') ->
        return' =<< gets _xmlState_changedPaths

    (Encrypt _next')       -> undefined

getNodeName :: NTZipper XNode -> Maybe Text
getNodeName z = getName' $ fromNTZipper z
    where
        getName' (NTree (XTag name _) _) = Just $ T.pack $ unXN $ localPart' name
        getName' _ = Nothing

mGetPath :: Maybe (NTZipper XNode) -> Path
mGetPath = maybe (error "xml: path of Nothing") getPath

getPath :: NTZipper XNode -> Path
getPath z = fst $ getPath' ([], z)
    where
        getPath' :: (Path, NTZipper XNode) -> (Path, NTZipper XNode)
        getPath' (p, z') = case up z' of
            Nothing -> (updatedP, z')
            Just uz -> getPath' (updatedP, uz)
            where
                updatedP = maybeToList (getNodeName z') <> p

mNext :: Maybe (NTZipper XNode) -> Maybe (NTZipper XNode)
mNext mntz = case mNextZ of
    Nothing -> Nothing
    Just nextZ -> if hasData nextZ
        then Just nextZ
        else mNext (Just nextZ)
    where
        mNextZ = mNext' True =<< mntz

mNext' :: Bool -> NTZipper XNode -> Maybe (NTZipper XNode)
mNext' isDown curZ =
    if isDown
        then down curZ <|> mNext' False curZ
        else toTheRight curZ <|> (mNext' False =<< up curZ)

hasData :: NTZipper XNode -> Bool
hasData z = hasData' xNode
    where
        NTree xNode _ = fromNTZipper z

        hasData' :: XNode -> Bool
        hasData' (XText t) = not $ all isSpace t
        hasData' (XTag _ attrs) = not $ null attrs
        hasData' _ = False

hashZipper :: Text -> NTZipper XNode -> NTZipper XNode
hashZipper k z = z { ntree = updatedNt }
    where
        NTree val children' = fromNTZipper z
        updatedNt = case val of
            XText t -> NTree (XText $ anonymize k t) children'
            _       -> NTree val children'

isMember :: Bloom String -> NTZipper XNode -> Bool
isMember bloom x = case val of
    XText t -> Bloom.elem (map toLower t) bloom
    _  ->  False
    where
        NTree val _children = fromNTZipper x

getRoot :: NTZipper XNode -> NTree XNode
getRoot z = fromNTZipper $ getRoot' z
    where
        getRoot' :: NTZipper XNode -> NTZipper XNode
        getRoot' z' = maybe z' getRoot' (up z')

addUpdatedPath :: (MonadState XMLState m, MonadIO m, MonadMask m)
               => Maybe (NTZipper XNode)
               -> m ()
addUpdatedPath mZipper = do
    changedPaths' <- gets _xmlState_changedPaths
    xmlState_changedPaths .= changedPaths' <> maybeToList (getPath <$> mZipper)
