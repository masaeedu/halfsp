{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module GhcideSteal (hoverInfo, symbolKindOfOccName) where

import Data.Array
import qualified Data.Map as M
import qualified Data.Text as T
import DynFlags
import GHC
import HieTypes
import HieUtils
import Language.LSP.Types
import Name
import Outputable hiding ((<>))
import SrcLoc

realSrcSpanToRange :: RealSrcSpan -> Range
realSrcSpanToRange real =
  Range
    (realSrcLocToPosition $ realSrcSpanStart real)
    (realSrcLocToPosition $ realSrcSpanEnd real)

realSrcLocToPosition :: RealSrcLoc -> Position
realSrcLocToPosition real =
  Position (srcLocLine real - 1) (srcLocCol real - 1)

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
        _ -> ["*Defined " <> T.pack (showSDocUnsafe $ pprNameDefnLoc name) <> "*"]

dropEnd1 :: [a] -> [a]
dropEnd1 [] = []
dropEnd1 (x : xs) = foldr (\z f y -> y : f z) (const []) xs x

symbolKindOfOccName :: OccName -> SymbolKind
symbolKindOfOccName defNameOcc
  | isVarOcc defNameOcc = SkVariable
  | isDataOcc defNameOcc = SkConstructor
  | isTcOcc defNameOcc = SkStruct
  | otherwise = SkUnknown 1
