
module Effect.Fixpoint where

import Control.Monad.Fix

import Core
import Effect.Final
import Effect.Embed

data Fixpoint m a where
  Fixpoint :: (a -> m a) -> Fixpoint m a

instance Effect Fixpoint where
  weave f (Fixpoint fp) = Fixpoint (f . fp)

instance Member Fixpoint fs => MonadFix (Eff fs) where
  mfix fp = send (Fixpoint fp)

asFixpoint
  :: forall m fs
  .  (Members [Final m, Embed m] fs, Diag fs fs, MonadFix m)
  => Eff (Fixpoint : fs)
  ~> Eff fs
asFixpoint = interpret \case
  Fixpoint fp -> do
    nfp <- final1 @m fp
    embed @m $ mfix nfp
