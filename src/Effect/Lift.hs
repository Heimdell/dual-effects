
{-|
  A weaker `Final`, only allows the liftding.
-}

module Effect.Lift
  ( -- * Interface
    Lift
  , lift

    -- * Implementation
  , liftToFinal
  , liftViaNat
    -- * Re-exporting core
  , module Core
  )
  where

import Control.Monad.IO.Class
import Data.Kind (Type)

import Core
import Effect.Final

-- | The message.
--
data Lift n (m :: Type -> Type) a where

  -- | Use the action from the final monad.
  --
  Lift :: n a -> Lift n m a
  deriving anyclass Effect

-- | Lift an action from the final monad.
lift :: forall n fs a. Members '[Lift n] fs => n a -> Eff fs a
lift na = send (Lift na)

instance Members '[Lift IO] fs => MonadIO (Eff fs) where
  liftIO = io

-- | Lift an action from `IO` monad.
io :: Members '[Lift IO] fs => IO ~> Eff fs
io act = send $ Lift $ liftIO @IO act

-- | Merge with `Final` action.
liftToFinal
  :: forall n fs
  .  Members '[Final n] fs
  => Eff (Lift n : fs)
  ~> Eff            fs
liftToFinal = plug \case
  Lift na -> lifts na

-- | Transform into some deeper `Lift`.
liftViaNat
  :: forall m n fs
  .  Members '[Lift n] fs
  => (m ~> n)
  -> Eff (Lift m : fs)
  ~> Eff fs
liftViaNat nat = plug \case
  Lift ma -> send $ Lift $ nat ma
