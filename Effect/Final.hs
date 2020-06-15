
module Effect.Final where

import Core

data Final n m a where
  Final  :: m a -> Final n m (n a)
  Final1 :: forall n m a x. (x -> m a) -> Final n m (x -> n a)
  Embeds :: n a -> Final n m a

runM :: forall m. Monad m => Eff '[Final m] ~> m
runM eff = runEff eff (handleFinal /\ empty)
  where
    handleFinal :: Final m m ~> m
    handleFinal = \case
      Final   na -> return na
      Embeds  na -> na
      Final1 ena -> return ena

instance Effect (Final n) where
  weave f (Final  ma) = Final  (f   ma)
  weave f (Final1 ma) = Final1 (f . ma)
  weave f (Embeds na) = Embeds na

final :: forall n fs a. Member (Final n) fs => Eff fs a -> Eff fs (n a)
final act = send (Final act)

final1
  :: forall n fs x a
  .  Member (Final n) fs
  => (x -> Eff fs a)
  ->  Eff fs (x -> n a)
final1 act = send (Final1 act)