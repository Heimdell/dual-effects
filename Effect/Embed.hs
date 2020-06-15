
module Effect.Embed where

import Core
import Effect.Final

data Embed n (m :: * -> *) a where
  Embed :: n a -> Embed n m a

instance Effect (Embed n) where
  weave f (Embed ma) = Embed ma

embed :: forall n fs a. Member (Embed n) fs => n a -> Eff fs a
embed na = send (Embed na)

embedToFinal
  :: forall n fs
  .  (Members '[Final n] fs)
  => Eff (Embed n : fs)
  ~> Eff            fs
embedToFinal = interpret \case
  Embed na -> send (Embeds na)