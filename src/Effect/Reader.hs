
{-|
  The `Reader` effect.
-}

module Effect.Reader
  ( -- * Interface
    Reader
  , ask
  , local

    -- * Implementations
  , asReader
  , mergeEnv

    -- * Re-exporting core
  , module Core
  )
  where

import qualified Control.Monad.Reader as MTL

import Core
import Effect.Final
import Effect.Lift
import Product

-- | The messages.
--
data Reader e m a where

  -- | Get current environment.
  --
  Ask      ::                    Reader e m e

  -- | Override the environment for some action.
  --
  Override :: (e -> e) -> m a -> Reader e m a

instance Effect (Reader e) where
  weave f  Ask            = Ask
  weave f (Override d ma) = Override d (f ma)

ask :: forall e fs. Member (Reader e) fs => Eff fs e
ask = send Ask

local :: forall e fs a. Member (Reader e) fs => (e -> e) -> Eff fs a -> Eff fs a
local d ma = send (Override d ma)

-- | Delegate the implementation to the `MonadReader` capabilities of the final
--   monad.
--
asReader
  :: forall e m fs
  .  (Members [Final m, Lift m] fs, Diag fs fs, MTL.MonadReader e m)
  => Eff (Reader e : fs)
  ~> Eff fs
asReader = interpret \case
  Ask -> lift @m MTL.ask
  Override f ma -> do
    nma <- final @m ma
    lift @m $ MTL.local f nma

-- | Delegate the implementation to the @Env (Product xs)@ effect, where
--   @xs@ contain the current environment @e@.
--
mergeEnv
  :: forall x xs fs
  .  (Contains x xs, Member (Reader (Product xs)) fs, Diag fs fs)
  => Eff (Reader x : fs)
  ~> Eff          fs
mergeEnv = interpret \case
  Ask -> do
    e <- ask @(Product xs)
    return (getElem e)

  Override f ma -> do
    local @(Product xs) (modElem f) ma
