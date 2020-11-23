
{-|
  The `MTL.WriterMonad`-like effect.

  I can't guarantee that if you route implementation via `Data.IORef.IORef` it will
  hold any properties if used in parallel processes.
-}

module Effect.Writer
  ( -- * Interface
    Writer
  , tell
  , listen

    -- * Implementation
  , writerToState

    -- * Re-exporting core
  , module Core
  )
  where

import Core
import Effect.Final
import Effect.State
import Data.IORef ()

-- | Ability to output messages.
data Writer w m a where
  Say       :: w   -> Writer w m ()
  Intercept :: m a -> Writer w m (w, a)

instance Effect (Writer w) where
  weave f (Say       w)  = Say w
  weave f (Intercept ma) = Intercept (f ma)

-- | Output a message.
tell :: forall w fs. Members '[Writer w] fs => w -> Eff fs ()
tell w = send (Say w)

-- | Intercept the messages the action sends.
listen :: forall w fs a. Members '[Writer w] fs => Eff fs a -> Eff fs (w, a)
listen act = send (Intercept act)

-- | Implement as `State`.
writerToState
  :: forall w fs
  .  (Members '[State w] fs, Monoid w)
  => Eff (Writer w : fs)
  ~> Eff fs
writerToState = plug \case
  Say w -> modify (w <>)
  Intercept ma -> do
    old <- get @w
    put @w mempty
    res <- ma
    new <- get
    modify (old <>)
    return (new, res)
