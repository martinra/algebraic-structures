module Math.Structure.Utils.TH
where

import Language.Haskell.TH


-- | Make instance of a type t for a class c
mkInstance :: TypeQ -> TypeQ -> DecQ
mkInstance t c = mkInstanceWith t c []

mkInstanceWith :: TypeQ -> TypeQ -> [Q Dec] -> DecQ
mkInstanceWith t c = instanceD (cxt []) (appT c t)

mkInstance' :: Name -> Name -> DecQ
mkInstance' t c = mkInstance (conT t) (conT c)

mkInstanceWith' :: Name -> Name -> [Q Dec] -> DecQ
mkInstanceWith' t c = mkInstanceWith (conT t) (conT c)

mkDecl :: Name -> Q Exp -> Q Dec
mkDecl f e = funD f [clause [] (normalB e ) []]

deriveInstance :: TypeQ -> TypeQ -> DecQ
deriveInstance t c = standaloneDerivD (cxt []) (appT c t)

deriveInstance' :: Name -> Name -> DecQ
deriveInstance' t c = deriveInstance (conT t) (conT c)

