{- | Resource management effect.
-}

module Effect.Resource
  ( -- * Interface
    Resource
  , protect
  , finally

    -- * Implementation
  , asMask

    -- * Re-exporting core
  , module Core
  )
  where

import Control.Monad.Catch hiding (finally)

import Core

import Effect.Final
import Effect.Lift

-- | Ability to allocate resources.
data Resource m a where
  Bracket :: forall a b c m. m a -> (a -> m c) -> (a -> m b) -> Resource m b

instance Effect Resource where
  weave f (Bracket alloc dealloc act) = Bracket (f alloc) (f . dealloc) (f . act)

-- | Allocate a resource, use it and then deallocate.
protect :: Members '[Resource] fs => Eff fs a -> (a -> Eff fs c) -> (a -> Eff fs b) -> Eff fs b
protect alloc dealloc act = send (Bracket alloc dealloc act)

-- | Perform second action even if first one raised an exception.
finally :: Members '[Resource] fs => Eff fs a -> Eff fs b -> Eff fs a
finally act dealloc = protect (pure ()) (const dealloc) (const act)

-- | Delegate to `MonadMask`.
asMask
  :: forall m fs
  .  (Members [Lift m, Final m] fs, MonadMask m)
  => Eff (Resource : fs)
  ~> Eff fs
asMask = plug \case
  Bracket alloc dealloc act -> do
    nalloc   <- final  @m alloc
    ndealloc <- final1 @m dealloc
    nact     <- final1 @m act
    lift @m $ bracket nalloc ndealloc nact
