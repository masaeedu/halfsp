{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module GhcideSteal (hoverInfo, symbolKindOfOccName, locationsAtPoint, gotoDefinition) where

import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Array
import Data.Containers.ListUtils (nubOrd)
import Data.List (isSuffixOf)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import DynFlags
import FastString (unpackFS)
import qualified FastString as FS
import GHC
import HieDb hiding (pointCommand)
import HieTypes
import HieUtils
import Language.LSP.Types
import MonadUtils (mapMaybeM)
import Name
import Outputable hiding ((<>))
import SrcLoc
import System.FilePath ((</>))

-- {{{ Development.IDE.Types.Location
--
-- }}}

-- {{{ Development.IDE.GHC.Error

realSrcSpanToRange :: RealSrcSpan -> Range
realSrcSpanToRange real =
  Range
    (realSrcLocToPosition $ realSrcSpanStart real)
    (realSrcLocToPosition $ realSrcSpanEnd real)

realSrcLocToPosition :: RealSrcLoc -> Position
realSrcLocToPosition real =
  Position (srcLocLine real - 1) (srcLocCol real - 1)

-- | Extract a file name from a GHC SrcSpan (use message for unhelpful ones)
-- FIXME This may not be an _absolute_ file name, needs fixing.
srcSpanToFilename :: SrcSpan -> Maybe FilePath
srcSpanToFilename (UnhelpfulSpan _) = Nothing
srcSpanToFilename (RealSrcSpan real) = Just $ FS.unpackFS $ srcSpanFile real

srcSpanToLocation :: FilePath -> SrcSpan -> Maybe Location
srcSpanToLocation wsroot src = do
  fs <- srcSpanToFilename src
  rng <- srcSpanToRange src
  pure $ Location (filePathToUri $ wsroot </> fs) rng

-- | Convert a GHC SrcSpan to a DAML compiler Range
srcSpanToRange :: SrcSpan -> Maybe Range
srcSpanToRange (UnhelpfulSpan _) = Nothing
srcSpanToRange (RealSrcSpan real) = Just $ realSrcSpanToRange real

-- }}}

showGhc :: Outputable a => DynFlags -> a -> T.Text
showGhc dflags = showSD dflags . ppr

showSD :: DynFlags -> SDoc -> T.Text
showSD dflags = T.pack . unsafePrintSDoc dflags

unsafePrintSDoc :: DynFlags -> SDoc -> String
unsafePrintSDoc dflags sdoc = renderWithStyle dflags sdoc (mkUserStyle dflags neverQualify AllTheWay)

showNameWithoutUniques :: Outputable a => DynFlags -> a -> T.Text
showNameWithoutUniques dflags = T.pack . prettyprint
  where
    dyn = dflags `gopt_set` Opt_SuppressUniques
    prettyprint x = renderWithStyle dyn (ppr x) style
    style = mkUserStyle dyn neverQualify AllTheWay

hoverInfo :: DynFlags -> Array TypeIndex HieTypeFlat -> HieAST TypeIndex -> (Maybe Range, [T.Text])
hoverInfo dflags typeLookup ast = (Just range, prettyNames ++ pTypes)
  where
    pTypes
      | length names == 1 = dropEnd1 $ map wrapHaskell prettyTypes
      | otherwise = map wrapHaskell prettyTypes

    range = realSrcSpanToRange $ nodeSpan ast

    wrapHaskell x = "\n```haskell\n" <> x <> "\n```\n"
    info = nodeInfo ast
    names = M.assocs $ nodeIdentifiers info
    types = nodeType info

    prettyNames :: [T.Text]
    prettyNames = map prettyName names
    prettyName (Right n, dets) =
      T.unlines $
        wrapHaskell (showNameWithoutUniques dflags n <> maybe "" (" :: " <>) (prettyType <$> identType dets)) :
        definedAt n
    prettyName (Left m, _) = showGhc dflags m

    prettyTypes = map (("_ :: " <>) . prettyType) types
    prettyType t = showGhc dflags $ hieTypeToIface $ recoverFullType t typeLookup

    definedAt name =
      -- do not show "at <no location info>" and similar messages
      -- see the code of 'pprNameDefnLoc' for more information
      case nameSrcLoc name of
        UnhelpfulLoc {} | isInternalName name || isSystemName name -> []
        _ -> ["*Defined " <> showSD dflags (pprNameDefnLoc name) <> "*"]

symbolKindOfOccName :: OccName -> SymbolKind
symbolKindOfOccName ocn
  | isVarOcc ocn = SkVariable
  | isDataOcc ocn = SkConstructor
  | isTcOcc ocn = SkStruct
  | otherwise = SkUnknown 1

gotoDefinition ::
  MonadIO m =>
  HieDb ->
  FilePath ->
  M.Map ModuleName Uri ->
  HieASTs a ->
  Position ->
  MaybeT m [Location]
gotoDefinition hiedb wsroot imports srcSpans pos =
  lift $ locationsAtPoint hiedb wsroot imports pos srcSpans

locationsAtPoint ::
  forall m a.
  MonadIO m =>
  HieDb ->
  FilePath ->
  M.Map ModuleName Uri ->
  Position ->
  HieASTs a ->
  m [Location]
locationsAtPoint hiedb wsroot imports pos ast =
  let ns = concat $ pointCommand ast pos (M.keys . nodeIdentifiers . nodeInfo)
      zeroPos = Position 0 0
      zeroRange = Range zeroPos zeroPos
      modToLocation m = (\fs -> pure $ Location fs zeroRange) <$> M.lookup m imports
   in nubOrd . concat <$> mapMaybeM (either (pure . modToLocation) $ nameToLocation hiedb wsroot) ns

pointCommand :: HieASTs t -> Position -> (HieAST t -> a) -> [a]
pointCommand hf pos k =
  catMaybes $
    M.elems $
      flip M.mapWithKey (getAsts hf) $ \fs ast ->
        case selectSmallestContaining (sp fs) ast of
          Nothing -> Nothing
          Just ast' -> Just $ k ast'
  where
    sloc fs = mkRealSrcLoc fs (line + 1) (cha + 1)
    sp fs = mkRealSrcSpan (sloc fs) (sloc fs)
    line = _line pos
    cha = _character pos

-- | Given a 'Name' attempt to find the location where it is defined.
nameToLocation :: MonadIO m => HieDb -> FilePath -> Name -> m (Maybe [Location])
nameToLocation hiedb wsroot name = runMaybeT $
  case nameSrcSpan name of
    sp@(RealSrcSpan rsp)
      -- Lookup in the db if we got a location in a boot file
      | not $ "boot" `isSuffixOf` unpackFS (srcSpanFile rsp) -> MaybeT $ pure $ pure <$> srcSpanToLocation wsroot sp
    sp -> do
      guard (sp /= wiredInSrcSpan)
      -- This case usually arises when the definition is in an external package.
      -- In this case the interface files contain garbage source spans
      -- so we instead read the .hie files to get useful source spans.
      mod <- MaybeT $ return $ nameModule_maybe name
      erow <- liftIO $ findDef hiedb (nameOccName name) (Just $ moduleName mod) (Just $ moduleUnitId mod)
      case erow of
        [] -> do
          -- If the lookup failed, try again without specifying a unit-id.
          -- This is a hack to make find definition work better with ghcide's nascent multi-component support,
          -- where names from a component that has been indexed in a previous session but not loaded in this
          -- session may end up with different unit ids
          erow <- liftIO $ findDef hiedb (nameOccName name) (Just $ moduleName mod) Nothing
          case erow of
            [] -> MaybeT $ pure Nothing
            xs -> lift $ mapMaybeM (runMaybeT . defRowToLocation wsroot) xs
        xs -> lift $ mapMaybeM (runMaybeT . defRowToLocation wsroot) xs

defRowToLocation :: Monad m => FilePath -> Res DefRow -> MaybeT m Location
defRowToLocation wsroot (row :. info) = do
  let start = Position (defSLine row - 1) (defSCol row - 1)
      end = Position (defELine row - 1) (defECol row - 1)
      range = Range start end
  file <- case modInfoSrcFile info of
    Just src -> pure $ filePathToUri $ wsroot </> src
    Nothing -> MaybeT $ pure Nothing
  pure $ Location file range

dropEnd1 :: [a] -> [a]
dropEnd1 [] = []
dropEnd1 (x : xs) = foldr (\z f y -> y : f z) (const []) xs x
