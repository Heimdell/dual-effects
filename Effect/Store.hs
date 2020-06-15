
module Effect.Store where

import Control.Monad.IO.Class
import Control.Monad.State

import Data.IORef

import Core
import Effect.Final
import Effect.Embed
import Effect.Env
import Product

data Store s (m :: * -> *) a where
  Retrieve :: Store s m s
  Store :: s -> Store s m ()

instance Effect (Store s) where
  weave f Retrieve = Retrieve
  weave f (Store s) = Store s

retrieve :: forall s fs. Member (Store s) fs => Eff fs s
retrieve = send Retrieve

store :: forall s fs. Member (Store s) fs => s -> Eff fs ()
store s = send (Store s)

change :: forall s fs. Member (Store s) fs => (s -> s) -> Eff fs ()
change f = store . f =<< retrieve

storeViaRIO
  :: forall e m fs
  .  (MonadIO m, Members [Env (IORef e), Embed m] fs, Diag fs fs)
  => Eff (Store e : fs)
  ~> Eff            fs
storeViaRIO = interpret \case
  Retrieve -> do
    ref <- env
    embed @m $ liftIO $ readIORef ref

  Store s -> do
    ref <- env
    embed @m $ liftIO $ writeIORef ref s

mergeStore
  :: forall x xs fs
  .  (Contains x xs, Member (Store (Product xs)) fs, Diag fs fs)
  => Eff (Store x : fs)
  ~> Eff            fs
mergeStore = interpret \case
  Retrieve -> do
    e <- retrieve @(Product xs)
    return (getElem e)

  Store s -> do
    change @(Product xs) $ modElem $ const s

asState
  :: forall e m fs
  .  (Members '[Embed m] fs, Diag fs fs, MonadState e m)
  => Eff (Store e : fs)
  ~> Eff fs
asState = interpret \case
  Retrieve -> embed @m get
  Store s  -> embed @m (put s)
