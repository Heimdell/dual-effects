
{-|
  The `Alternative` effect.

  I can't do the "return any alternative holding the result" as "polysemy" does,
  because I only `interpret` and it is not allowed to change the effect result.
-}

module Effect.NonDet
  ( -- * Interface
    NonDet
  , choose
  , loose

    -- * Implementation
  , asAlternative

    -- * Re-exporting core
  , module Core
  )
  where

import Control.Applicative

import Core
import Effect.Final
import Effect.Embed

-- The messages.
data NonDet m a where
  Loose  :: NonDet m a
  Choose :: m a -> m a -> NonDet m a

instance Effect NonDet where
  weave f  Loose       = Loose
  weave f (Choose a b) = Choose (f a) (f b)

-- | An `empty`.
loose :: Member NonDet fs => Eff fs a
loose = send Loose

-- | A `(<|>)`.
choose :: Member NonDet fs => Eff fs a -> Eff fs a -> Eff fs a
choose a b = send (Choose a b)

instance Member NonDet fs => Alternative (Eff fs) where
  empty = loose
  (<|>) = choose

-- | Delegate to the final monad.
asAlternative
  :: forall m fs
  .  (Members [Final m, Embed m] fs, Diag fs fs, Alternative m)
  => Eff (NonDet : fs)
  ~> Eff fs
asAlternative = interpret \case
  Loose -> embed @m $ empty
  Choose a b -> do
    na <- final @m a
    nb <- final @m b
    embed @m $ na <|> nb

