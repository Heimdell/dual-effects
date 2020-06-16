
module Effect.Embed where

import Core
import Effect.Final

data Embed n (m :: * -> *) a where
  Embed :: n a -> Embed n m a
  deriving anyclass Effect

embed :: forall n fs a. Member (Embed n) fs => n a -> Eff fs a
embed na = send (Embed na)

embedToFinal
  :: forall n fs
  .  (Members '[Final n] fs, Diag fs fs)
  => Eff (Embed n : fs)
  ~> Eff            fs
embedToFinal = interpret \case
  Embed na -> send (Embeds na)