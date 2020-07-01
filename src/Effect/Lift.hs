
{-|
  A weaker `Final`, only allows the liftding.
-}

module Effect.Lift
  -- ( -- * Interface
  --   Lift
  -- , lift

  --   -- * Implementation
  -- , liftToFinal
  -- , liftViaNat
  --   -- * Re-exporting core
  -- , module Core
  -- )
  where

import Control.Monad.IO.Class

import Core
import Effect.Final

-- | The message.
--
data Lift n (m :: * -> *) a where

  -- | Use the action from the final monad.
  --
  Lift :: n a -> Lift n m a
  deriving anyclass Effect

lift :: forall n fs a. Member (Lift n) fs => n a -> Eff fs a
lift na = send (Lift na)

instance (Member (Lift IO) fs, Diag fs fs) => MonadIO (Eff fs) where
  liftIO = io

io :: (Member (Lift IO) fs) => IO ~> Eff fs
io act = send $ Lift $ liftIO @IO act

liftToFinal
  :: forall n fs
  .  (Member (Final n) fs, Diag fs fs)
  => Eff (Lift n : fs)
  ~> Eff            fs
liftToFinal = interpret \case
  Lift na -> lifts na

liftViaNat
  :: forall m n fs
  .  (Member (Lift n) fs, Diag fs fs)
  => (m ~> n)
  -> Eff (Lift m : fs)
  ~> Eff fs
liftViaNat nat = interpret \case
  Lift ma -> send $ Lift $ nat ma
