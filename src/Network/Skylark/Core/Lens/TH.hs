{-# LANGUAGE LambdaCase #-}

module Network.Skylark.Core.Lens.TH
  ( makeClassyConstraints
  ) where

import Control.Lens
import Language.Haskell.TH
import Network.Skylark.Core.Prelude

makeClassyConstraints :: Name -> [Name] -> DecsQ
makeClassyConstraints name names = do
  decls <- makeClassy name
  return $ addConstraints names decls

addConstraints :: [Name] -> [Dec] -> [Dec]
addConstraints names = \case
  ClassD cs n tvs f d : ds ->
    ClassD (newConstraints names tvs ++ cs) n tvs f d : ds
  ds -> ds

newConstraints :: [Name] -> [TyVarBndr] -> [Type]
newConstraints ns =
  loop where
    loop = \case
      PlainTV name : _tvs ->
        flip map ns $ \n ->
          AppT (ConT n) (VarT name)
      _tv : tvs -> loop tvs
      [] -> []
