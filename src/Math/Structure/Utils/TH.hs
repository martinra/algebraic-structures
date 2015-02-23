module Math.Structure.Utils.TH
where

import Language.Haskell.TH


-- | Make instance of a type t for a class c
mkInstance :: Name -> Name -> DecQ
mkInstance t c =
  -- TyConI tinfo <- reify t
  -- TyConI cinfo <- reify c
  -- (tname,_,_,_) <- typeInfo $ return tinfo
  -- (cname,_,_,_) <- typeInfo $ return cinfo
  instanceD (cxt []) (appT (conT c) (conT t)) []
