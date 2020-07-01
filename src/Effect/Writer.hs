
{-|
  The `WriterMonad` effect.

  I can't guarantee that if you route implementation via `IORef` it will
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

data Writer w m a where
  Say       :: w   -> Writer w m ()
  Intercept :: m a -> Writer w m (w, a)

instance Effect (Writer w) where
  weave f (Say       w)  = Say w
  weave f (Intercept ma) = Intercept (f ma)

tell :: forall w fs. Member (Writer w) fs => w -> Eff fs ()
tell w = send (Say w)

listen :: forall w fs a. Member (Writer w) fs => Eff fs a -> Eff fs (w, a)
listen act = send (Intercept act)

writerToState
  :: forall w fs
  .  (Member (State w) fs, Diag fs fs, Monoid w)
  => Eff (Writer w : fs)
  ~> Eff fs
writerToState = interpret \case
  Say w -> modify (w <>)
  Intercept ma -> do
    old <- get @w
    put @w mempty
    res <- ma
    new <- get
    modify (old <>)
    return (new, res)
