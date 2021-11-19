{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Lib (serverMain) where

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Bifunctor (Bifunctor (first, second))
import Data.Coerce
import Data.Foldable (asum)
import Data.Functor ((<&>))
import Data.Functor.Compose
import Data.Kind
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (listToMaybe)
import Data.Monoid (Endo (..))
import Data.String.Conversions
import Data.Text (Text, intercalate, pack, replace, unpack)
import DynFlags (DynFlags)
import GhcideSteal (gotoDefinition, hoverInfo, symbolKindOfOccName)
import HIE.Bios
  ( CradleLoadResult (CradleFail, CradleNone, CradleSuccess),
    loadCradle,
  )
import HIE.Bios.Environment (getRuntimeGhcLibDir)
import HieDb (ModuleInfo (modInfoName), dynFlagsForPrinting, getAllIndexedMods, hieModInfo, pointCommand, searchDef, withHieDb, withTarget, type (:.) ((:.)))
import HieDb.Types
  ( DefRow (..),
    HieDb,
    HieDbErr (..),
    HieTarget,
    LibDir (LibDir),
    Res,
  )
import HieTypes (HieAST, HieFile (hie_asts, hie_types), TypeIndex)
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
  ( DefinitionParams (..),
    ErrorCode (InternalError, InvalidRequest),
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
    SMethod (STextDocumentDefinition, STextDocumentHover, SWorkspaceSymbol),
    SymbolInformation (..),
    TextDocumentIdentifier (..),
    Uri,
    WorkspaceSymbolParams (WorkspaceSymbolParams, _query),
    filePathToUri,
    sectionSeparator,
    uriToFilePath,
    type (|?) (InL, InR),
  )
import Language.LSP.Types.Lens (uri)
import Lens.Micro ((^.))
import Module (ModuleName, moduleNameSlashes, moduleNameString)
import OccName
  ( occNameString,
  )
import System.Directory (doesFileExist)
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

-- {{{ hacky garbage that needs to be replaced

hardcodedSourceDirs :: [Text]
hardcodedSourceDirs = ["src", "test"]

replaceMany :: [Text] -> Text -> Text -> Text
replaceMany patterns substitution = appEndo . foldMap (Endo . flip replace substitution) $ patterns

-- Less dumb alternative compose
type LDAC :: (Type -> Type) -> (Type -> Type) -> Type -> Type
newtype LDAC f g a = LDAC (f (g a))
  deriving (Functor)
  deriving (Applicative) via (Compose f g)

instance (Applicative f, Alternative g) => Alternative (LDAC f g) where
  empty = LDAC $ pure empty
  LDAC x <|> LDAC y = LDAC $ liftA2 (<|>) x y

whicheverOfManyThingsWorks
  :: (Applicative f, Applicative g, Alternative h)
  => [f (g (h b))]
  -> f (g (h b))
whicheverOfManyThingsWorks = coerce . asum . fmap (LDAC . LDAC)

-- TODO: Make this less hacky, involves fixing up the NULL entries in the modules table in hiedb
textDocumentIdentifierToHieFilePath :: TextDocumentIdentifier -> HieTarget
textDocumentIdentifierToHieFilePath (TextDocumentIdentifier u) =
  Left
  $ unpack
  $ replace ".hs" ".hie"
  $ replaceMany hardcodedSourceDirs ".hiefiles"
  $ pack
  $ fromJust
  $ uriToFilePath u

fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 = fmap . fmap

-- TODO: Get rid of this hack, defer to populated modules table
moduleToTextDocumentIdentifier :: FilePath -> ModuleInfo -> IO (Maybe TextDocumentIdentifier)
moduleToTextDocumentIdentifier wsroot = fmap2 (TextDocumentIdentifier . filePathToUri) . whicheverOfManyThingsWorks (fmap (moduleFileInSourceDirIfExists . cs) hardcodedSourceDirs)
  where
    moduleFileInSourceDirIfExists :: FilePath -> ModuleInfo -> IO (Maybe FilePath)
    moduleFileInSourceDirIfExists sourceDir = ensureFilePathExists . (wsroot </>) . (sourceDir </>) . (<.> "hs") . moduleNameSlashes . modInfoName

    ensureFilePathExists :: FilePath -> IO (Maybe FilePath)
    ensureFilePathExists path = doesFileExist path <&> \b -> if b then Just path else Nothing

-- }}}

astsAtPoint :: HieFile -> (Int, Int) -> Maybe (Int, Int) -> [HieAST TypeIndex]
astsAtPoint hiefile start end = pointCommand hiefile start end id

hieFileFromTextDocumentIdentifier :: HieDb -> TextDocumentIdentifier -> IO (Either ResponseError HieFile)
hieFileFromTextDocumentIdentifier hiedb tdocId =
  first hiedbErrorToResponseError <$> withTarget hiedb (textDocumentIdentifierToHieFilePath tdocId) id

hieFileAndAstsFromPointRequest :: HieDb -> TextDocumentIdentifier -> Position -> IO (Either ResponseError (HieFile, [HieAST TypeIndex]))
hieFileAndAstsFromPointRequest hiedb tdocId position =
  hieFileFromTextDocumentIdentifier hiedb tdocId `etbind` \hiefile ->
    etpure (hiefile, astsAtPoint hiefile (coordsLSPToHieDb position) Nothing)

hieFileAndAstFromPointRequest :: HieDb -> TextDocumentIdentifier -> Position -> IO (Either ResponseError (HieFile, Maybe (HieAST TypeIndex)))
hieFileAndAstFromPointRequest hiedb tdocId position =
  fmap (second listToMaybe) <$> hieFileAndAstsFromPointRequest hiedb tdocId position

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

moduleSourcePathMap :: HieDb -> FilePath -> IO (Map ModuleName Uri)
moduleSourcePathMap hiedb wsroot = do
  rows <- getAllIndexedMods hiedb
  m <- sequenceA $ M.fromList $ fmap ((modInfoName &&& moduleToTextDocumentIdentifier wsroot) . hieModInfo) rows
  pure $ (M.mapMaybe . fmap) (^. uri) $ m

-- The actual code

symbolInfo :: FilePath -> Res DefRow -> IO SymbolInformation
symbolInfo wsroot (DefRow {..} :. m) = do
  mtdi <- moduleToTextDocumentIdentifier wsroot m
  case mtdi of
    Nothing -> fail "unable to find text document corresponding to module"
    Just tdi ->
      pure $
        SymbolInformation
          { _name = pack $ occNameString defNameOcc,
            _kind = symbolKindOfOccName defNameOcc,
            _deprecated = Nothing,
            _location =
              Location
                { _uri = tdi ^. uri,
                  _range =
                    Range
                      { _start = coordsHieDbToLSP (defSLine, defSCol),
                        _end = coordsHieDbToLSP (defELine, defECol)
                      }
                },
            _containerName = Just $ pack $ moduleNameString $ modInfoName m
          }

handleWorkspaceSymbolRequest :: Handlers (LspT c IO)
handleWorkspaceSymbolRequest = requestHandler SWorkspaceSymbol $ \req ->
  getWsRoot `ekbind` \wsroot -> eklift . kcodensity . liftIO $ do
    results <- withHieDb (wsroot </> ".hiedb") $ flip searchDef $ unpack $ requestQuery req
    prepareSymbolResults wsroot results
  where
    prepareSymbolResults :: FilePath -> [Res DefRow] -> IO (List SymbolInformation)
    prepareSymbolResults wsroot = fmap List . traverse (symbolInfo wsroot)

    requestQuery :: Message 'WorkspaceSymbol -> Text
    requestQuery RequestMessage {_params = WorkspaceSymbolParams {_query = q}} = q

retrieveHoverData :: HieDb -> DynFlags -> Message 'TextDocumentHover -> IO (Either ResponseError (Maybe Hover))
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
          getDynFlags wsroot `etbind` \dflags -> retrieveHoverData hiedb dflags req

handleDefinitionRequest :: Handlers (LspT c IO)
handleDefinitionRequest = requestHandler STextDocumentDefinition $ \RequestMessage {_params = DefinitionParams {..}} ->
  getWsRoot `ekbind` \wsroot ->
    kliftIO $
      withHieDb (wsroot </> ".hiedb") `kbind` \hiedb ->
        kcodensity $
          hieFileFromTextDocumentIdentifier hiedb _textDocument `etbind` \hiefile -> do
            modmap <- moduleSourcePathMap hiedb wsroot
            result <- runMaybeT $ gotoDefinition hiedb wsroot modmap (hie_asts hiefile) _position
            pure $ case result of
              Nothing -> Left $ ResponseError InternalError "Unable to go to definition" Nothing
              Just l -> Right $ InR $ InL $ List l

serverDef :: ServerDefinition ()
serverDef =
  ServerDefinition
    { onConfigurationChange = const $ pure $ Left "Changing configuration is not supported",
      doInitialize = pure . pure . pure,
      staticHandlers =
        mconcat
          [ handleWorkspaceSymbolRequest,
            handleTextDocumentHoverRequest,
            handleDefinitionRequest
          ],
      interpretHandler = \env -> Iso (runLspT env) liftIO,
      options = defaultOptions
    }

serverMain :: IO Int
serverMain = runServer serverDef
