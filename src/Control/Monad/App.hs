{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
module Control.Monad.App where

import           Control.Monad.IO.Unlift

import           Control.Monad.Reader

import           Data.Environment        (Env)

newtype AppM a = AppM (ReaderT Env IO a)

deriving instance Functor AppM
deriving instance Applicative AppM
deriving instance Monad AppM
deriving instance MonadIO AppM
deriving instance MonadReader Env AppM
deriving instance MonadUnliftIO AppM

runAppM :: Env -> AppM a -> IO a
runAppM env (AppM f) = runReaderT f env
