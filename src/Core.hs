
{-|
  The core of the effect system. /Service locator/ via /interpreter/.

  The @Eff fs a@ is a computation tree, consisting of effects @fs@ in any
  structure or combination, with a result of type @a@.

  The `Eff` is internally a function from dispatchers list to any monad.
  The type is parametrised with a list of effects. It contains effects by
  "closing over" them by using `send` function, that transforms some effect
  into a computation tree.

  The call of `send` denotes a place for invocation of some service.

  A `Dispatch` is a list of transformations from each of the prepared effects to
  a concrete monad.

  A prepared effect is the one with all recursive occurences of `Eff` are
  already replaced with the output monad.

  The `Effect` interface provides `weave` function that allows to do that.

  To run the effect, use either `runEff` with dispatchers or
  `Effect.Final.runM`.
-}

module Core
  ( -- * Carrier
    Eff
  , runEff
  , plug
  , send

    -- * Properties
  , Effect (..)
  , Members

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

-- | For all @x@, a type of function from @f x@ to @g x@.
--
--   Used to hide the @x@ type variable for clarity.
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
newtype Eff fs a = Eff
  { -- | Convert to any monad via `Dispatch`.
    runEff :: forall m. Monad m => Dispatch fs m -> m a
  }

instance Functor (Eff fs) where
  fmap = liftM

instance Applicative (Eff fs) where
  pure a = Eff \_ -> pure a
  (<*>) = ap

instance Monad (Eff fs) where
  Eff run >>= callb = Eff $ \d -> do
    a <- run d
    callb a `runEff` d

type family Members_ fs gs :: Constraint where
  Members_ '[]       gs = ()
  Members_  (f : fs) gs = (Member f gs, Members_ fs gs)

-- | Checks if all of @fs@ effects are present in @gs@.
--
type Members fs gs = (Members_ fs gs, Diag gs gs)

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
class Effect (f :: (* -> *) -> * -> *) where
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
{-# INLINE plug #-}
plug
  :: forall f fs
  .  (Effect f, Diag fs fs)
  => (f  (Eff fs) ~> Eff fs)
  -> Eff (f : fs) ~> Eff fs
plug retract = (`runEff` (retract :/\ diag))

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
