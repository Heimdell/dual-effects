
{-|
  The `MTL.MonadState`-like effect.
-}

module Effect.State
  ( -- * Interface
    State
  , get
  , gets
  , put
  , modify

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

-- | Ability to have `MTL.State` @s@.
data State s (m :: * -> *) a where
  Get ::      State s m s
  Put :: s -> State s m ()
  deriving anyclass Effect

-- | Get current state.
get :: forall s fs. Members '[State s] fs => Eff fs s
get = send Get

-- | Get view on the current state.
gets :: forall s a fs. Members '[State s] fs => (s -> a) -> Eff fs a
gets f = f <$> get

-- | Replace the state.
put :: forall s fs. Members '[State s] fs => s -> Eff fs ()
put s = send (Put s)

-- | Modify the state.
modify :: forall s fs. Members '[State s] fs => (s -> s) -> Eff fs ()
modify f = put . f =<< get

-- | Implement via `IORef`.
storeViaRIO
  :: forall e m fs
  .  (MonadIO m, Members [Reader (IORef e), Lift m] fs)
  => Eff (State e : fs)
  ~> Eff            fs
storeViaRIO = plug \case
  Get -> do
    ref <- ask
    lift @m $ liftIO $ readIORef ref

  Put s -> do
    ref <- ask
    lift @m $ liftIO $ writeIORef ref s

-- | Implement like `mergeEnv`.
mergeState
  :: forall x xs fs
  .  (Contains x xs, Members '[State (Product xs)] fs)
  => Eff (State x : fs)
  ~> Eff            fs
mergeState = plug \case
  Get -> do
    gets @(Product xs) getElem

  Put s -> do
    modify @(Product xs) $ modElem $ const s

-- | Delegate to final monad.
asState
  :: forall e m fs
  .  (Members '[Lift m] fs, MTL.MonadState e m)
  => Eff (State e : fs)
  ~> Eff fs
asState = plug \case
  Get   -> lift @m  MTL.get
  Put s -> lift @m (MTL.put s)
