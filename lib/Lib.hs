{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Lib where

import Control.Arrow ((&&&))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bifunctor (Bifunctor (bimap))
import Data.Text (Text, intercalate, pack, replace, unpack)
import DynFlags (DynFlags)
import GHC (srcSpanEndCol, srcSpanEndLine, srcSpanStartCol)
import HIE.Bios
  ( CradleLoadResult (CradleFail, CradleNone, CradleSuccess),
    loadCradle,
  )
import HIE.Bios.Environment (getRuntimeGhcLibDir)
import HieDb
  ( dynFlagsForPrinting,
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
    ModuleInfo
      ( ModuleInfo,
        modInfoHash,
        modInfoIsBoot,
        modInfoIsReal,
        modInfoName,
        modInfoSrcFile,
        modInfoUnit
      ),
    Res,
  )
import HieTypes (HieAST (nodeInfo, nodeSpan), HieFile (hie_types), NodeInfo (nodeType), Span, TypeIndex)
import HieUtils (recoverFullType, renderHieType)
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
    SymbolKind (SkConstructor, SkInterface, SkUnknown, SkVariable),
    WorkspaceSymbolParams (WorkspaceSymbolParams, _query),
    filePathToUri,
    uriToFilePath,
  )
import Language.LSP.Types.Lens (uri)
import Lens.Micro ((^.))
import Module (moduleNameSlashes, moduleNameString)
import OccName
  ( OccName,
    isDataOcc,
    isTcOcc,
    isVarOcc,
    occNameString,
  )
import SrcLoc (srcSpanStartLine)
import System.FilePath ((<.>), (</>))
import Utils ( EK, fromJust, ekbind, eklift, kcodensity )

-- LSP utils
getWsRoot :: MonadLsp cfg m => EK (m r) (m r) ResponseError FilePath
getWsRoot = kcodensity $ do
  mRootPath <- getRootPath
  pure $ case mRootPath of
    Nothing -> Left $ ResponseError InvalidRequest "No root workspace was found" Nothing
    Just p -> Right p

getDynFlags :: MonadLsp cfg m => FilePath -> EK (m r) (m r) ResponseError DynFlags
getDynFlags wsroot cb = do
  cradle <- liftIO $ loadCradle (wsroot </> "hie.yaml")
  mlibdir <- liftIO $ getRuntimeGhcLibDir cradle
  case mlibdir of
    CradleSuccess libdir -> do
      dflags <- liftIO $ dynFlagsForPrinting $ LibDir libdir
      cb $ Right dflags
    CradleFail e -> cb $ Left $ ResponseError InternalError (pack $ show e) Nothing
    CradleNone -> cb $ Left $ ResponseError InternalError "No cradle available" Nothing

-- HieDb utils
withTarget' :: HieTarget -> (HieFile -> a) -> HieDb -> IO (Either HieDbErr a)
withTarget' tgt cb hiedb = withTarget hiedb tgt cb

withNode' :: (Int, Int) -> Maybe (Int, Int) -> (HieAST TypeIndex -> a) -> HieFile -> [a]
withNode' s e cb f = pointCommand f s e cb

hiedbLocationToPosition :: (Int, Int) -> Position
hiedbLocationToPosition (l, c) = Position {_line = l - 1, _character = c - 1}

positionToHiedbLocation :: Position -> (Int, Int)
positionToHiedbLocation Position {..} = (_line + 1, _character + 1)

-- GHC utils
symbolKindOfOccName :: OccName -> SymbolKind
symbolKindOfOccName n
  | isDataOcc n = SkConstructor
  | isVarOcc n = SkVariable
  | isTcOcc n = SkInterface
  | otherwise = SkUnknown 0

ghcSpanToLspRange :: HieTypes.Span -> Range
ghcSpanToLspRange = uncurry Range . (startLoc &&& endLoc)
  where
    startLoc = hiedbLocationToPosition . (srcSpanStartLine &&& srcSpanStartCol)
    endLoc = hiedbLocationToPosition . (srcSpanEndLine &&& srcSpanEndCol)

-- The actual code
symbolInfo :: FilePath -> Res DefRow -> SymbolInformation
symbolInfo workspaceRootPath (DefRow {..} :. ModuleInfo {..}) =
  SymbolInformation
    { _name = pack $ occNameString defNameOcc,
      _kind = symbolKindOfOccName defNameOcc,
      _deprecated = Nothing,
      _location =
        Location
          { _uri = filePathToUri $ (workspaceRootPath </>) $ ("src" </>) $ (<.> "hs") $ moduleNameSlashes modInfoName,
            _range =
              Range
                { _start = hiedbLocationToPosition (defSLine, defSCol),
                  _end = hiedbLocationToPosition (defELine, defECol)
                }
          },
      _containerName = Just $ pack $ moduleNameString modInfoName
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

getTypesAtLocation :: DynFlags -> HieFile -> HieAST TypeIndex -> [Text]
getTypesAtLocation dynflags hiefile ast = fmap (pack . renderHieType dynflags . (`recoverFullType` hie_types hiefile)) $ nodeType $ nodeInfo ast

-- TODO: Fill this out properly
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

getTypesAtPoint :: DynFlags -> FilePath -> Message 'TextDocumentHover -> IO (Either HieDbErr [(HieAST TypeIndex, [Text])])
getTypesAtPoint dynflags wsroot RequestMessage {_params = HoverParams {..}} = do
  writeFile "/tmp/log" hiefilePath
  withHieDb (wsroot </> ".hiedb") . withTarget' (Left hiefilePath) $
    \hiefile ->
      pointCommand
        hiefile
        (positionToHiedbLocation _position)
        Nothing
        $ \ast ->
          (ast, getTypesAtLocation dynflags hiefile ast)
  where
    hiefilePath :: FilePath
    hiefilePath = unpack $ replace ".hs" ".hie" $ replace "src" ".hiefiles" $ pack $ fromJust $ uriToFilePath $ _textDocument ^. uri

handleTextDocumentHoverRequest :: Handlers (LspT c IO)
handleTextDocumentHoverRequest = requestHandler STextDocumentHover $ \req ->
  getWsRoot `ekbind` \wsroot ->
    getDynFlags wsroot `ekbind` \dflags ->
      kcodensity . liftIO $ do
        results <- getTypesAtPoint dflags wsroot req
        pure $ bimap hiedbErrorToResponseError prepareHoverResults results
  where
    prepareHoverResults :: [(HieAST TypeIndex, [Text])] -> Maybe Hover
    prepareHoverResults = \case
      [] -> Nothing
      -- TODO: See if we can do something with the remaining ASTs
      ((ast, renderedTypes) : _) -> Just $ Hover (HoverContents $ MarkupContent MkMarkdown $ intercalate "\n" $ fmap ("- " <>) renderedTypes) $ Just $ ghcSpanToLspRange $ nodeSpan ast

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
