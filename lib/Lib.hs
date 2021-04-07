{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Lib (serverMain) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bifunctor (Bifunctor (first, second))
import Data.Maybe (listToMaybe)
import Data.Text (Text, intercalate, pack, replace, unpack)
import DynFlags (DynFlags)
import GhcideSteal (hoverInfo, symbolKindOfOccName)
import HIE.Bios
  ( CradleLoadResult (CradleFail, CradleNone, CradleSuccess),
    loadCradle,
  )
import HIE.Bios.Environment (getRuntimeGhcLibDir)
import HieDb
  ( ModuleInfo (modInfoName),
    dynFlagsForPrinting,
    pointCommand,
    searchDef,
    withHieDb,
    withTarget,
    type (:.) ((:.)),
  )
import HieDb.Types
  ( DefRow (..),
    HieDb,
    HieDbErr (..),
    HieTarget,
    LibDir (LibDir),
    Res,
  )
import HieTypes (HieAST, HieFile (hie_types), TypeIndex)
import Language.LSP.Server
  ( Handlers,
    LspT,
    MonadLsp,
    ServerDefinition (..),
    defaultOptions,
    getRootPath,
    requestHandler,
    runLspT,
    runServer,
    type (<~>) (Iso),
  )
import Language.LSP.Types
  ( ErrorCode (InternalError, InvalidRequest),
    Hover (..),
    HoverContents (..),
    HoverParams (..),
    List (List),
    Location (Location, _range, _uri),
    MarkupContent (..),
    MarkupKind (MkMarkdown),
    Message,
    Method (TextDocumentHover, WorkspaceSymbol),
    Position (Position, _character, _line),
    Range (Range, _end, _start),
    RequestMessage (RequestMessage, _params),
    ResponseError (ResponseError),
    SMethod (STextDocumentHover, SWorkspaceSymbol),
    SymbolInformation (..),
    TextDocumentIdentifier (..),
    WorkspaceSymbolParams (WorkspaceSymbolParams, _query),
    filePathToUri,
    sectionSeparator,
    uriToFilePath,
  )
import Language.LSP.Types.Lens (uri)
import Lens.Micro ((^.))
import Module (moduleNameSlashes, moduleNameString)
import OccName
  ( occNameString,
  )
import System.FilePath ((<.>), (</>))
import Utils (EK, ekbind, eklift, etbind, etpure, fromJust, kbind, kcodensity, kliftIO)

-- LSP utils
getWsRoot :: MonadLsp cfg m => EK (m r) (m r) ResponseError FilePath
getWsRoot = kcodensity $ do
  mRootPath <- getRootPath
  pure $ case mRootPath of
    Nothing -> Left $ ResponseError InvalidRequest "No root workspace was found" Nothing
    Just p -> Right p

-- TODO: Store the hiedb location, cradle, and GHC libdir on startup instead of reading them on every request

-- TODO: Retrieve the user's configured flags using hie-bios instead of using HieDb.Utils.dynFlagsForPrinting
getDynFlags :: MonadIO m => FilePath -> m (Either ResponseError DynFlags)
getDynFlags wsroot = liftIO $ do
  cradle <- loadCradle (wsroot </> "hie.yaml")
  mlibdir <- getRuntimeGhcLibDir cradle
  case mlibdir of
    CradleSuccess libdir -> do
      dflags <- dynFlagsForPrinting $ LibDir libdir
      pure $ Right dflags
    CradleFail e -> pure $ Left $ ResponseError InternalError (pack $ show e) Nothing
    CradleNone -> pure $ Left $ ResponseError InternalError "No cradle available" Nothing

-- HieDb utils
coordsHieDbToLSP :: (Int, Int) -> Position
coordsHieDbToLSP (l, c) = Position {_line = l - 1, _character = c - 1}

coordsLSPToHieDb :: Position -> (Int, Int)
coordsLSPToHieDb Position {..} = (_line + 1, _character + 1)

-- TODO: Make this less hacky, involves fixing up the NULL entries in the modules table in hiedb
textDocumentIdentifierToHieFilePath :: TextDocumentIdentifier -> HieTarget
textDocumentIdentifierToHieFilePath (TextDocumentIdentifier u) =
  Left $ unpack $ replace ".hs" ".hie" $ replace "src" ".hiefiles" $ pack $ fromJust $ uriToFilePath u

-- TODO: Get rid of this hack, defer to populated modules table
moduleToTextDocumentIdentifier :: FilePath -> ModuleInfo -> TextDocumentIdentifier
moduleToTextDocumentIdentifier wsroot = TextDocumentIdentifier . filePathToUri . (wsroot </>) . ("src" </>) . (<.> "hs") . moduleNameSlashes . modInfoName

astsAtPoint :: HieFile -> (Int, Int) -> Maybe (Int, Int) -> [HieAST TypeIndex]
astsAtPoint hiefile start end = pointCommand hiefile start end id

astsForRequest :: HieDb -> TextDocumentIdentifier -> Position -> IO (Either HieDbErr (HieFile, [HieAST TypeIndex]))
astsForRequest hiedb tdocId position =
  withTarget hiedb (textDocumentIdentifierToHieFilePath tdocId) $ \hiefile ->
    (hiefile, astsAtPoint hiefile (coordsLSPToHieDb position) Nothing)

hieFileAndAstFromPointRequest :: HieDb -> TextDocumentIdentifier -> Position -> IO (Either HieDbErr (HieFile, Maybe (HieAST TypeIndex)))
hieFileAndAstFromPointRequest hiedb tdocId position = fmap (second listToMaybe) <$> astsForRequest hiedb tdocId position

-- TODO: Render these properly
renderHieDbError :: HieDbErr -> Text
renderHieDbError =
  pack . \case
    NotIndexed modname munitid -> "not indexed"
    AmbiguousUnitId modinfo -> "ambiguous unit id"
    NameNotFound occname mmodname munitid -> "name not found"
    NoNameAtPoint tgt hiepos -> "no name at point"
    NameUnhelpfulSpan name str -> "name unhelpful span"

hiedbErrorToResponseError :: HieDbErr -> ResponseError
hiedbErrorToResponseError = flip (ResponseError InternalError) Nothing . renderHieDbError

-- The actual code

symbolInfo :: FilePath -> Res DefRow -> SymbolInformation
symbolInfo wsroot (DefRow {..} :. mod) =
  SymbolInformation
    { _name = pack $ occNameString defNameOcc,
      _kind = symbolKindOfOccName defNameOcc,
      _deprecated = Nothing,
      _location =
        Location
          { _uri = moduleToTextDocumentIdentifier wsroot mod ^. uri,
            _range =
              Range
                { _start = coordsHieDbToLSP (defSLine, defSCol),
                  _end = coordsHieDbToLSP (defELine, defECol)
                }
          },
      _containerName = Just $ pack $ moduleNameString $ modInfoName mod
    }

handleWorkspaceSymbolRequest :: Handlers (LspT c IO)
handleWorkspaceSymbolRequest = requestHandler SWorkspaceSymbol $ \req ->
  getWsRoot `ekbind` \wsroot -> eklift . kcodensity . liftIO $ do
    results <- withHieDb (wsroot </> ".hiedb") $ flip searchDef $ unpack $ requestQuery req
    pure $ prepareSymbolResults wsroot results
  where
    prepareSymbolResults :: FilePath -> [Res DefRow] -> List SymbolInformation
    prepareSymbolResults wsroot = List . fmap (symbolInfo wsroot)

    requestQuery :: Message 'WorkspaceSymbol -> Text
    requestQuery RequestMessage {_params = WorkspaceSymbolParams {_query = q}} = q

retrieveHoverData :: HieDb -> DynFlags -> Message 'TextDocumentHover -> IO (Either HieDbErr (Maybe Hover))
retrieveHoverData hiedb dflags RequestMessage {_params = HoverParams {..}} =
  hieFileAndAstFromPointRequest hiedb _textDocument _position `etbind` \(hiefile, mast) -> case mast of
    Nothing -> etpure Nothing
    Just ast -> etpure $
      Just $ case hoverInfo dflags (hie_types hiefile) ast of
        (mbrange, contents) ->
          Hover
            { _range = mbrange,
              _contents = HoverContents $ MarkupContent MkMarkdown $ intercalate sectionSeparator contents
            }

handleTextDocumentHoverRequest :: Handlers (LspT c IO)
handleTextDocumentHoverRequest = requestHandler STextDocumentHover $ \req ->
  getWsRoot `ekbind` \wsroot ->
    kliftIO $
      withHieDb (wsroot </> ".hiedb") `kbind` \hiedb ->
        kcodensity $
          getDynFlags wsroot `etbind` \dflags -> do
            hdata <- retrieveHoverData hiedb dflags req
            pure $ first hiedbErrorToResponseError hdata

serverDef :: ServerDefinition ()
serverDef =
  ServerDefinition
    { onConfigurationChange = const $ pure $ Left "Changing configuration is not supported",
      doInitialize = pure . pure . pure,
      staticHandlers = mconcat [handleWorkspaceSymbolRequest, handleTextDocumentHoverRequest],
      interpretHandler = \env -> Iso (runLspT env) liftIO,
      options = defaultOptions
    }

serverMain :: IO Int
serverMain = runServer serverDef
