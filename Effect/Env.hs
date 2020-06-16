
{-|
  The `Reader` effect.
-}

module Effect.Env where

import Control.Monad.Reader

import Core
import Effect.Final
import Effect.Embed
import Product

-- | The messages.
--
data Env e m a where

  -- | Get current environment.
  --
  Env      ::                    Env e m e

  -- | Override the environment for some action.
  --
  Override :: (e -> e) -> m a -> Env e m a

instance Effect (Env e) where
  weave f  Env            = Env
  weave f (Override d ma) = Override d (f ma)

env :: forall e fs. Member (Env e) fs => Eff fs e
env = send Env

override :: forall e fs a. Member (Env e) fs => (e -> e) -> Eff fs a -> Eff fs a
override d ma = send (Override d ma)

-- | Delegate the implementation to the `MonadReader` capabilities of the final
--   monad.
--
asReader
  :: forall e m fs
  .  (Members [Final m, Embed m] fs, Diag fs fs, MonadReader e m)
  => Eff (Env e : fs)
  ~> Eff fs
asReader = interpret \case
  Env -> embed @m ask
  Override f ma -> do
    nma <- final @m ma
    embed @m $ local f nma

-- | Delegate the implementation to the @Env (Product xs)@ effect, where
--   @xs@ contain the current environment @e@.
--
mergeEnv
  :: forall x xs fs
  .  (Contains x xs, Member (Env (Product xs)) fs, Diag fs fs)
  => Eff (Env x : fs)
  ~> Eff          fs
mergeEnv = interpret \case
  Env -> do
    e <- env @(Product xs)
    return (getElem e)

  Override f ma -> do
    override @(Product xs) (modElem f) ma
