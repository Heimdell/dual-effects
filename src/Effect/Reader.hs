
{-|
  The `Reader` effect.
-}

module Effect.Reader
  ( -- * Interface
    Reader
  , ask
  , asks
  , local

    -- * Implementations
  , asReader
  , mergeEnv
  , runRIO
  , runIOWith

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

-- | Ask for current environment.
ask :: forall e fs. Members '[Reader e] fs => Eff fs e
ask = send Ask

-- | Ask for view on the current environment.
asks :: forall e a fs. Members '[Reader e] fs => (e -> a) -> Eff fs a
asks f = f <$> ask

-- | Change the environment fot the computation.
local :: forall e fs a. Members '[Reader e] fs => (e -> e) -> Eff fs a -> Eff fs a
local d ma = send (Override d ma)

-- | Delegate the implementation to the `MTL.MonadReader` capabilities of the final
--   monad.
--
asReader
  :: forall e m fs
  .  (Members [Final m, Lift m] fs, MTL.MonadReader e m)
  => Eff (Reader e : fs)
  ~> Eff fs
asReader = plug \case
  Ask -> lift @m MTL.ask
  Override f ma -> do
    nma <- final @m ma
    lift @m $ MTL.local f nma

-- | Delegate the implementation to the @Env (Product xs)@ effect, where
--   @xs@ contain the current environment @e@.
--
mergeEnv
  :: forall x xs fs
  .  (Contains x xs, Members '[Reader (Product xs)] fs)
  => Eff (Reader x : fs)
  ~> Eff          fs
mergeEnv = plug \case
  Ask -> do
    asks @(Product xs) getElem

  Override f ma -> do
    local @(Product xs) (modElem f) ma

-- | Run as @RIO@.
runRIO
  :: forall e a
  .  Eff [Reader e, Final (MTL.ReaderT e IO)] a
  -> MTL.ReaderT e IO a
runRIO = runM . runReader
  where
    runReader = plug \case
      Ask -> lifts @(MTL.ReaderT e IO) MTL.ask
      Override f ma -> do
        nma <- final @(MTL.ReaderT e IO) ma
        lifts @(MTL.ReaderT e IO) $ MTL.local f nma

-- | Run as @IO@, shortcutting the environment into `MTL.ReaderT`.
runIOWith
  :: forall e a
  .  e
  -> Eff [Reader e, Final (MTL.ReaderT e IO)] a
  -> IO a
runIOWith e = (`MTL.runReaderT` e) . runRIO
