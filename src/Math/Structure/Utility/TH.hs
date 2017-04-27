module Math.Structure.Utility.TH
where

import Language.Haskell.TH


-- | Make instance of a type t for a class c
mkInstance :: CxtQ -> TypeQ -> TypeQ -> DecQ
mkInstance cxt t c = mkInstanceWith cxt t c []

mkInstanceWith :: CxtQ -> TypeQ -> TypeQ -> [Q Dec] -> DecQ
mkInstanceWith cxt t c = instanceD cxt (appT c t)

-- mkInstance' :: Name -> Name -> DecQ
-- mkInstance' t c = mkInstance (conT t) (conT c)
-- 
-- mkInstanceWith' :: Name -> Name -> [Q Dec] -> DecQ
-- mkInstanceWith' t c = mkInstanceWith (conT t) (conT c)

mkDecl :: Name -> ExpQ -> DecQ
mkDecl f e = funD f [clause [] (normalB e ) []]

deriveInstance :: CxtQ -> TypeQ -> TypeQ -> DecQ
deriveInstance cxt t c = standaloneDerivD cxt (appT c t)

-- deriveInstance' :: Name -> Name -> DecQ
-- deriveInstance' t c = deriveInstance (conT t) (conT c)

