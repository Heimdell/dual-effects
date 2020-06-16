
module Effect.NonDet where

import Control.Applicative

import Core
import Effect.Final
import Effect.Embed

data NonDet m a where
  Empty  :: NonDet m a
  Choose :: m a -> m a -> NonDet m a

instance Effect NonDet where
  weave f  Empty       = Empty
  weave f (Choose a b) = Choose (f a) (f b)

stop :: Member NonDet fs => Eff fs a
stop = send Empty

choose :: Member NonDet fs => Eff fs a -> Eff fs a -> Eff fs a
choose a b = send (Choose a b)

instance Member NonDet fs => Alternative (Eff fs) where
  empty = stop
  (<|>) = choose

asAlternative
  :: forall m fs
  .  (Members [Final m, Embed m] fs, Diag fs fs, Alternative m)
  => Eff (NonDet : fs)
  ~> Eff fs
asAlternative = interpret \case
  Empty -> embed @m $ empty
  Choose a b -> do
    na <- final @m a
    nb <- final @m b
    embed @m $ na <|> nb

