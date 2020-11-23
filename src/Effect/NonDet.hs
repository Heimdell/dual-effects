
{-|
  The `Alternative` effect.

  I can't do the "return any alternative holding the result" as "polysemy" does,
  because I only have `Core.plug` and it is not allowed to change the effect result.
-}

module Effect.NonDet
  ( -- * Interface
    NonDet

    -- * Implementation
  , asAlternative

    -- * Re-exporting core
  , module Core
  )
  where

import Control.Applicative

import Core
import Effect.Final
import Effect.Lift

-- | The messages.
--
--   Use `empty`/`<|>`.
--
data NonDet m a where
  Loose  :: NonDet m a
  Choose :: m a -> m a -> NonDet m a

instance Effect NonDet where
  weave f  Loose       = Loose
  weave f (Choose a b) = Choose (f a) (f b)

instance Members '[NonDet] fs => Alternative (Eff fs) where
  empty   = send Loose
  a <|> b = send (Choose a b)

-- | Delegate to the final monad.
asAlternative
  :: forall m fs
  .  (Members [Final m, Lift m] fs, Alternative m)
  => Eff (NonDet : fs)
  ~> Eff fs
asAlternative = plug \case
  Loose -> lift @m $ empty
  Choose a b -> do
    na <- final @m a
    nb <- final @m b
    lift @m $ na <|> nb
