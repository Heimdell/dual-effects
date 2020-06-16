
{-|
  A weaker `Final`, only allows the embedding.
-}

module Effect.Embed where

import Control.Monad.IO.Class

import Core
import Effect.Final

-- | The message.
--
data Embed n (m :: * -> *) a where

  -- | Use the action from the final monad.
  --
  Embed :: n a -> Embed n m a
  deriving anyclass Effect

embed :: forall n fs a. Member (Embed n) fs => n a -> Eff fs a
embed na = send (Embed na)

instance (Member (Embed IO) fs, Diag fs fs) => MonadIO (Eff fs) where
  liftIO = io

io :: (Member (Embed IO) fs) => IO ~> Eff fs
io act = send $ Embed $ liftIO @IO act

embedToFinal
  :: forall n fs
  .  (Members '[Final n] fs, Diag fs fs)
  => Eff (Embed n : fs)
  ~> Eff            fs
embedToFinal = interpret \case
  Embed na -> send (Embeds na)

embedViaNat
  :: forall m n fs
  .  (Members '[Embed n] fs, Diag fs fs)
  => (m ~> n)
  -> Eff (Embed m : fs)
  ~> Eff fs
embedViaNat nat = interpret \case
  Embed ma -> send $ Embed $ nat ma
