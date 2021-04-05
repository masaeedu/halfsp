{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lib where

import Control.Monad.IO.Class
import Data.Text (pack, unpack)
import HieDb
import Language.LSP.Server
import Language.LSP.Types
import Module
import OccName
import System.FilePath

symbolKindOfOccName :: OccName -> SymbolKind
symbolKindOfOccName n
  | isDataOcc n = SkConstructor
  | isVarOcc n = SkVariable
  | isTcOcc n = SkInterface
  | otherwise = SkUnknown 0

symbolInfo :: FilePath -> Res DefRow -> SymbolInformation
symbolInfo rootPath (DefRow {..} :. ModuleInfo {..}) =
  SymbolInformation
    { _name = pack $ occNameString defNameOcc,
      _kind = symbolKindOfOccName defNameOcc,
      _deprecated = Nothing,
      _location =
        Location
          { _uri = filePathToUri $ (rootPath </>) $ ("src" </>) $ (<.> "hs") $ moduleNameSlashes modInfoName,
            _range =
              Range
                { _start = Position {_line = defSLine - 1, _character = defSCol - 1},
                  _end = Position {_line = defELine - 1, _character = defECol - 1}
                }
          },
      _containerName = Just $ pack $ moduleNameString modInfoName
    }

handleWorkspaceSymbolRequest :: Handlers (LspT c IO)
handleWorkspaceSymbolRequest = requestHandler SWorkspaceSymbol $ \RequestMessage {_params = WorkspaceSymbolParams {_query = query}} cb -> do
  mRootPath <- getRootPath
  case mRootPath of
    Nothing -> cb $ Left $ ResponseError InvalidRequest "No root workspace was found" Nothing
    Just rootPath -> do
      results <- liftIO $ withHieDb (rootPath </> ".hiedb") $ flip searchDef $ unpack query
      cb $ Right $ List $ fmap (symbolInfo rootPath) results

serverDef :: ServerDefinition ()
serverDef =
  ServerDefinition
    { onConfigurationChange = const $ pure $ Left "Changing configuration is not supported",
      doInitialize = pure . pure . pure,
      staticHandlers = handleWorkspaceSymbolRequest,
      interpretHandler = \env -> Iso (runLspT env) liftIO,
      options = defaultOptions
    }

serverMain :: IO Int
serverMain = runServer serverDef
