
{-|
  The `WriterMonad` effect.

  I can't guarantee that if you route implementation via `IORef` it will
  hold any properties if used in parallel processes.
-}

module Effect.Write
  ( -- * Interface
    Write
  , say
  , intercept

    -- * Implementation
  , writeToStore

    -- * Re-exporting core
  , module Core
  )
  where

import Core
import Effect.Final
import Effect.Embed
import Effect.Store

data Write w m a where
  Say       :: w   -> Write w m ()
  Intercept :: m a -> Write w m (w, a)

instance Effect (Write w) where
  weave f (Say       w)  = Say w
  weave f (Intercept ma) = Intercept (f ma)

say :: forall w fs. Member (Write w) fs => w -> Eff fs ()
say w = send (Say w)

intercept :: forall w fs a. Member (Write w) fs => Eff fs a -> Eff fs (w, a)
intercept act = send (Intercept act)

writeToStore
  :: forall w fs
  .  (Member (Store w) fs, Diag fs fs, Monoid w)
  => Eff (Write w : fs)
  ~> Eff fs
writeToStore = interpret \case
  Say w -> change (w <>)
  Intercept ma -> do
    old <- retrieve @w
    store @w mempty
    res <- ma
    new <- retrieve
    store old
    return (new, res)

-- ignoreWrite
--   :: forall w fs
--   .  (Monoid w, Diag fs fs)
--   => Eff (Write w : fs)
--   ~> Eff fs
-- ignoreWrite = interpret \case
--   Say       _  -> return ()
--   Intercept ma -> do
--     a <- ma
--     return (mempty, a)