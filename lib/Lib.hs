{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lib where

import Control.Monad.IO.Class
import Data.Text (pack, unpack)
import HieDb
import Language.LSP.Server
import Language.LSP.Types
import OccName
import Module
import System.FilePath

symbolInfo :: FilePath -> Res DefRow -> SymbolInformation
symbolInfo rootPath (DefRow {..} :. ModuleInfo {..}) =
  SymbolInformation
    { _name = pack $ occNameString defNameOcc,
      _kind =
        if isDataOcc defNameOcc
          then SkConstructor
          else
            if isVarOcc defNameOcc
              then SkVariable
              else
                if isTcOcc defNameOcc
                  then SkInterface
                  else SkUnknown 0,
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

serverDef :: ServerDefinition ()
serverDef =
  ServerDefinition
    { onConfigurationChange = const $ pure $ Left "Changing configuration is not supported",
      doInitialize = pure . pure . pure,
      staticHandlers =
        mconcat
          [ requestHandler SWorkspaceSymbol $
              \RequestMessage {_params = WorkspaceSymbolParams {_query = query}} cb -> do
                mRootPath <- getRootPath
                case mRootPath of
                  Nothing -> cb $ Left $ ResponseError InvalidRequest "No root workspace was found" Nothing
                  Just rootPath -> do
                    results <- liftIO $ withHieDb (rootPath </> ".hiedb") $ flip searchDef $ unpack query
                    cb $ Right $ List $ fmap (symbolInfo rootPath) results
          ],
      interpretHandler = \env -> Iso (runLspT env) liftIO,
      options = defaultOptions
    }

serverMain :: IO Int
serverMain = runServer serverDef
