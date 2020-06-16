
{-|
  The core of the effect system.

  In my previous attempt, Eff = (Union fs ~> m) ~> m. Yet Union was only
  mentioned in `send` function, and the only thing was possible to do with it
  was to `dispatch`. Now the `Member` class encapsulates the Union+dispatch
  ideas.

  The Eff is a `Dispatch fs m ~> m`, where `Dispatch` is a list of handlers
  for effects in the @fs@ type list.

  The solvable problem of current implementation is that `Dispatch` is a /list/,
  and therefore incurs @O(N)@ penality to traverse.

  Another problem is the `Diag` property, which makes it possible to interpret
  any effect list into itself. The `diag` dispatcher is constructed iteratively
  and isn't inlinable. If the `Dispatch` was a `f m ~> m /\ Union fs m ~> m`
  instead, it would be (probably) possible to use `id` instead of `diag`.

  In previous implementation, I used to `weave` out recursive references to the
  same effect in `interpret`. But now `send` does that, running everything into
  the final monad at once. This, somehow, doesn't break neither Reader nor
  Error effects.
-}

module Core
  ( -- * Carrier
    Eff
  , runEff
  , run
  , interpret
  , expand
  , send

    -- * Properties
  , Effect (..)
  , Member
  , Members
  , Diag

    -- * Interpreter
  , Dispatch
  , (/\)
  , skip

    -- * Utils
  , type (~>)
  )
  where

import Control.Monad (liftM, ap)
import Control.Monad.Fix

import Data.Coerce (Coercible, coerce)
import Data.Kind (Constraint)

-- | Natural transformation. Used to hide the @x@ type variable.
--
type f ~> g = forall x. f x -> g x

-- | A list of effect interpreters.
--
data Dispatch fs m where
  (:/\) :: f m ~> m -> Dispatch fs m -> Dispatch (f : fs) m
  Empty :: Dispatch '[] m

-- | Add an interpreter to the list.
--
(/\) :: f m ~> m -> Dispatch fs m -> Dispatch (f : fs) m
(/\) = (:/\)

-- | An empty list of interpreters.
--
skip :: Dispatch '[] m
skip = Empty

-- | The effect carrier.
--
newtype Eff fs a = Eff { runEff :: forall m. Monad m => Dispatch fs m -> m a }

instance Functor (Eff fs) where
  fmap = liftM

instance Applicative (Eff fs) where
  pure a = Eff \_ -> pure a
  (<*>) = ap

instance Monad (Eff fs) where
  Eff run >>= callb = Eff $ \d -> do
    a <- run d
    callb a `runEff` d

-- | Checks if all of @fs@ effects are present in @gs@.
--
type family Members fs gs :: Constraint where
  Members '[]       gs = ()
  Members  (f : fs) gs = (Member f gs, Members fs gs)

-- | Checks if @f@ effect is present in @fs@.
--
class Member f fs where
  dispatch :: Dispatch fs m -> f m ~> m

instance {-# OVERLAPS #-} Member f (f : fs) where
  dispatch (unF :/\ _) f = unF f

instance Member f fs => Member f (g : fs) where
  dispatch (_ :/\ rest) f = dispatch rest f

-- | Wrap a concrete effect into carrier.
--
--   It essentially asks the interpreter to do something.
--
{-# INLINE send #-}
send :: forall f fs. (Effect f, Member f fs) => f (Eff fs) ~> Eff fs
send fs = Eff \d -> dispatch d $ weave (`runEff` d) fs

-- | An ability to do second-order effects.
--
class Effect f where
  weave :: (n ~> m) -> (f n ~> f m)

  -- | First-order effects can be inferred with "anyclass".
  --
  default weave
    :: (forall x. Coercible (f n x) (f m x))
    => (n ~> m)
    -> (f n ~> f m)
  weave _ = coerce

-- | Every effect library should have this one.
--
run :: Monad m => Eff '[] ~> m
run = (`runEff` skip)

-- | Peel off one effect at a time.
--
{-# INLINE interpret #-}
interpret
  :: forall f fs
  .  (Effect f, Diag fs fs)
  => (f  (Eff fs) ~> Eff fs)
  -> Eff (f : fs) ~> Eff fs
interpret retract = (`runEff` (retract :/\ diag))

-- | Make a @Eff gs@ out of @Eff fs@.
--
expand :: Diag fs gs => Eff fs ~> Eff gs
expand = (`runEff` diag)

-- | Ability to make a @Eff gs@ out of @Eff fs@.
--
class Diag fs gs where
  diag :: Dispatch fs (Eff gs)

instance Diag '[] gs where
  diag = Empty

instance (Diag fs gs, Effect f, Member f gs) => Diag (f : fs) gs where
  diag = send :/\ diag
