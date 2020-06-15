module Core
  ( type (~>)
  , Dispatch
  , (/\)
  , empty
  , Eff
  , runEff
  , send
  , Member
  , Members
  , Effect (..)
  , interpret
  , Diag
  )
  where

import Control.Monad (liftM, ap)

import Data.Coerce (Coercible)
import Data.Kind (Constraint)

import Unsafe.Coerce (unsafeCoerce)

type f ~> g = forall x. f x -> g x

data Dispatch fs m where
  (:/\) :: f m ~> m -> Dispatch fs m -> Dispatch (f : fs) m
  Empty :: Dispatch '[] m

(/\) :: f m ~> m -> Dispatch fs m -> Dispatch (f : fs) m
(/\) = (:/\)

empty :: Dispatch '[] m
empty = Empty

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

type Members fs gs = (Members' fs gs, Diag gs gs)

type family Members' fs gs :: Constraint where
  Members' '[]       gs = ()
  Members'  (f : fs) gs = (Member f gs, Members' fs gs)

class Member f fs where
  dispatch :: Dispatch fs m -> f m ~> m

instance {-# OVERLAPS #-} Member f (f : fs) where
  dispatch (unF :/\ _) f = unF f

instance Member f fs => Member f (g : fs) where
  dispatch (_ :/\ rest) f = dispatch rest f

send :: forall f fs. (Effect f, Member f fs) => f (Eff fs) ~> Eff fs
send fs = Eff \d -> dispatch d $ weave (`runEff` d) fs

class Effect f where
  weave :: (n ~> m) -> (f n ~> f m)

interpret
  :: forall f fs x
  .  (Effect f, Diag fs fs)
  => (f  (Eff fs) ~> Eff fs)
  -> Eff (f : fs) ~> Eff fs
interpret retract = go
  where
    go :: forall x. Eff (f : fs) x -> Eff fs x
    go = (`runEff` (retract :/\ diag))

class Diag fs gs where
  diag :: Dispatch fs (Eff gs)

instance Diag '[] gs where
  diag = Empty

instance (Diag fs gs, Effect f, Member f gs) => Diag (f : fs) gs where
  diag = send :/\ diag
