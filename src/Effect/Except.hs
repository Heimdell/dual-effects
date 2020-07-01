
{-|
  The `MonadThrow`/`MonadCatch` effect.
-}

module Effect.Except
  ( -- * Interface
    Except
  -- , raise
  -- , handle

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

-- | The messages.
--
--   Use `throwM`/`catch`.
--
data Except m a where
  Raise  :: Exception e => e -> Except m a
  Handle :: Exception e => m a -> (e -> m a) -> Except m a

instance Effect Except where
  weave f (Raise e)       = Raise e
  weave f (Handle ma ema) = Handle (f ma) (f . ema)

-- raise :: forall e fs a. (Exception e, Member Except fs) => e -> Eff fs a
-- raise e = send (Raise e)

-- handle
--   :: forall e fs a
--   .  (Member Except fs, Exception e)
--   => Eff fs a
--   -> (e -> Eff fs a)
--   -> Eff fs a
-- handle ma ema = send (Handle ma ema)

-- | Delegate `Exception` handling to the `Final` monad.
--
errorViaCatch
  :: forall m fs
  .  (MonadCatch m, Members '[Lift m, Final m] fs, Diag fs fs)
  => Eff (Except : fs)
  ~> Eff          fs
errorViaCatch = interpret \case
  Raise  e      -> lift @m (throwM e)
  Handle ma ema -> do
    nma  <- final  @m ma
    nema <- final1 @m ema
    lift $ catch nma nema

instance Member Except fs => MonadThrow (Eff fs) where
  throwM e = send (Raise e)

instance Member Except fs => MonadCatch (Eff fs) where
  catch ma ema = send (Handle ma ema)