module Data.Store where

import           Control.Lens    (Iso', Lens', iso, lens, set, view)
import           Data.Text       (Text)
import           Data.Text.Lens  (unpacked)
import           System.FilePath

newtype Store = Store FilePath

_Store :: Iso' Store FilePath
_Store = iso (\(Store s) -> s) Store

class HasStore a where
  {-# MINIMAL getStore, setStore | store #-}
  getStore :: a -> Store
  getStore = view store

  setStore :: a -> Store -> a
  setStore = flip (set store)

  store :: Lens' a Store
  store = lens getStore setStore

pathForName :: Store -> Text -> FilePath
pathForName s n = view _Store s </> view unpacked n <.> ".gpg"
