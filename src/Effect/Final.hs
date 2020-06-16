
{-|
  The last effect in almost every list.
  Holds the "final monad" whole computation will be interpreted in.

  Since the ouly place the `Final` can be interpreted is the last one,
  the "current" monad becomes the "final" one become the same thing and
  therefore the interpretation is trivial. `Lifts` just does the action and
  both `Final` and `Final1` just return them.

  The fact that `Final` is nominal in @n@ and can't be made a `Functor`
  prevents me from making a `finalToFinal` method, like one in "polysemy".
-}

module Effect.Final
  ( -- * Interface
    Final
  , final
  , final1
  , lifts

    -- * Runners
  , runM

    -- * Re-exporting core
  , module Core
  )
  where

import Unsafe.Coerce

import Core

-- | The messages.
data Final n m a where

  -- | Turn some current-monad action into the final monad one.
  --
  Final  ::                       m a  -> Final n m (n a)

  -- | Same as `Final`, but with (potentially tupled) arguments.
  --
  Final1 :: forall n m a x. (x -> m a) -> Final n m (x -> n a)

  -- | Embed an action from final-monad into current monad.
  --
  Lifts  ::                       n a  -> Final n m a

-- fmapFinal :: (s ~> n) -> Final n m a -> Final s m a
-- fmapFinal nat (Final ma) = unsafeCoerce $ Final ma

-- weaveFinal :: (n ~> s) -> Final n m a -> Final s m a
-- weaveFinal forth = \case
--   Final   ma -> unsafeCoerce $ Final ma
--   Final1 ema -> unsafeCoerce $ Final1 ema
--   Lifts  na -> Lifts $ forth na

-- | If `Final m` the single remaining effect, convert the action into @m@.
--
runM :: forall m. Monad m => Eff '[Final m] ~> m
runM eff = runEff eff (handleFinal /\ skip)
  where
    handleFinal :: Final m m ~> m
    handleFinal = \case
      Final   na -> return na
      Lifts  na -> na
      Final1 ena -> return ena

-- finalViaNat
--   :: forall m n fs
--   .  (Member (Final n) fs, Diag fs fs)
--   => m ~> n
--   -> Eff (Final m : fs)
--   ~> Eff fs
-- finalViaNat nat = interpret $ send . weaveFinal nat

instance Effect (Final n) where
  weave f (Final  ma) = Final  (f   ma)
  weave f (Final1 ma) = Final1 (f . ma)
  weave f (Lifts na) = Lifts na

final :: forall n fs a. Member (Final n) fs => Eff fs a -> Eff fs (n a)
final act = send (Final act)

final1
  :: forall n fs x a
  .  Member (Final n) fs
  => (x -> Eff fs a)
  -> Eff fs (x -> n a)
final1 act = send (Final1 act)

lifts :: forall n fs a. Member (Final n) fs => n a -> Eff fs a
lifts na = send (Lifts na)