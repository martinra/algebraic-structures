{-# LANGUAGE
    TemplateHaskell
  #-}

module Math.Structure.Utils.TH
where

import Language.Haskell.TH


-- | Make instance of a type t for a class c
mkInstance :: Name -> Name -> DecQ
mkInstance t c = mkInstanceWith t c []

mkInstanceWith :: Name -> Name -> [Q Dec] -> DecQ
mkInstanceWith t c ds =
  instanceD (cxt []) (appT (conT c) (conT t)) ds

mkDecl :: Name -> Q Exp -> Q Dec
mkDecl f e = funD f [clause [] (normalB e ) []]
