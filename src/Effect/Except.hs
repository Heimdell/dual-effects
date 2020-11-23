
{-|
  The `MonadThrow`/`MonadCatch` effect.
-}

module Effect.Except
  ( -- * Interface
    Except

    -- * Implementations
  , errorViaCatch

    -- * Re-exporting core
  , module Core
  )
  where

import Control.Monad.Catch hiding (handle)

import Core
import Effect.Final
import Effect.Lift

-- | Ability to throw/catch exceptions.
--
--   Use `throwM`/`catch`.
--
data Except m a where
  Raise  :: Exception e => e -> Except m a
  Handle :: Exception e => m a -> (e -> m a) -> Except m a

instance Effect Except where
  weave f (Raise e)       = Raise e
  weave f (Handle ma ema) = Handle (f ma) (f . ema)

-- | Delegate `Exception` handling to the `Final` monad.
--
errorViaCatch
  :: forall m fs
  .  (Members '[Lift m, Final m] fs, MonadCatch m)
  => Eff (Except : fs)
  ~> Eff           fs
errorViaCatch = plug \case
  Raise  e      -> lift @m (throwM e)
  Handle ma ema -> do
    nma  <- final  @m ma
    nema <- final1 @m ema
    lift $ catch nma nema

instance Members '[Except] fs => MonadThrow (Eff fs) where throwM e      = send (Raise  e)
instance Members '[Except] fs => MonadCatch (Eff fs) where catch  ma ema = send (Handle ma ema)
