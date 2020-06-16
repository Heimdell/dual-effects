
{-|
  The `MonadState` effect.
-}

module Effect.State
  ( -- * Interface
    State
  , retrieve
  , store
  , change

    -- * Implementations
  , storeViaRIO
  , mergeState
  , asState

    -- * Re-exporting core
  , module Core
  )
  where

import Control.Monad.IO.Class
import qualified Control.Monad.State as MTL

import Data.IORef
import Data.Coerce

import Core
import Effect.Final
import Effect.Lift
import Effect.Reader
import Product

data State s (m :: * -> *) a where
  Get ::      State s m s
  Put :: s -> State s m ()
  deriving anyclass Effect

retrieve :: forall s fs. Member (State s) fs => Eff fs s
retrieve = send Get

store :: forall s fs. Member (State s) fs => s -> Eff fs ()
store s = send (Put s)

change :: forall s fs. Member (State s) fs => (s -> s) -> Eff fs ()
change f = store . f =<< retrieve

-- | Implement via `IORef`.
storeViaRIO
  :: forall e m fs
  .  (MonadIO m, Members [Reader (IORef e), Lift m] fs, Diag fs fs)
  => Eff (State e : fs)
  ~> Eff            fs
storeViaRIO = interpret \case
  Get -> do
    ref <- ask
    lift @m $ liftIO $ readIORef ref

  Put s -> do
    ref <- ask
    lift @m $ liftIO $ writeIORef ref s

-- | Implement like `mergeEnv`.
mergeState
  :: forall x xs fs
  .  (Contains x xs, Member (State (Product xs)) fs, Diag fs fs)
  => Eff (State x : fs)
  ~> Eff            fs
mergeState = interpret \case
  Get -> do
    e <- retrieve @(Product xs)
    return (getElem e)

  Put s -> do
    change @(Product xs) $ modElem $ const s

-- | Delegate to final monad.
asState
  :: forall e m fs
  .  (Members '[Lift m] fs, Diag fs fs, MTL.MonadState e m)
  => Eff (State e : fs)
  ~> Eff fs
asState = interpret \case
  Get   -> lift @m  MTL.get
  Put s -> lift @m (MTL.put s)
