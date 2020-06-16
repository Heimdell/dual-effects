
module Effect.Resource where

import Control.Monad.Catch

import Core

import Effect.Final
import Effect.Embed

data Resource m a where
  Bracket :: forall a b c m. m a -> (a -> m c) -> (a -> m b) -> Resource m b

instance Effect Resource where
  weave f (Bracket alloc dealloc act) = Bracket (f alloc) (f . dealloc) (f . act)

protect :: Member Resource fs => Eff fs a -> (a -> Eff fs c) -> (a -> Eff fs b) -> Eff fs b
protect alloc dealloc act = send (Bracket alloc dealloc act)

finally :: Member Resource fs => Eff fs a -> Eff fs b -> Eff fs a
finally act dealloc = protect (pure ()) (const dealloc) (const act)

asMask
  :: forall m fs
  .  (MonadMask m, Members [Embed m, Final m] fs, Diag fs fs)
  => Eff (Resource : fs)
  ~> Eff fs
asMask = interpret \case
  Bracket alloc dealloc act -> do
    nalloc   <- final  @m alloc
    ndealloc <- final1 @m dealloc
    nact     <- final1 @m act
    embed @m $ bracket nalloc ndealloc nact