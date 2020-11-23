
{-|
  The `MonadFix` effect.
-}

module Effect.Fixpoint
  ( -- * Interface
    Fixpoint

    -- * Implementation
  , asFixpoint

    -- * Re-exporting core
  , module Core
  )
  where

import Control.Monad.Fix

import Core
import Effect.Final
import Effect.Lift

-- | The message.
--
--   Use `mfix`.
--
data Fixpoint m a where
  Fixpoint :: (a -> m a) -> Fixpoint m a

instance Effect Fixpoint where
  weave f (Fixpoint fp) = Fixpoint (f . fp)

instance Members '[Fixpoint] fs => MonadFix (Eff fs) where
  mfix fp = send (Fixpoint fp)

-- | Delegate to the `Final` monad.
asFixpoint
  :: forall m fs
  .  (Members [Final m, Lift m] fs, MonadFix m)
  => Eff (Fixpoint : fs)
  ~> Eff fs
asFixpoint = plug \case
  Fixpoint fp -> do
    nfp <- final1 @m fp
    lift @m $ mfix nfp
