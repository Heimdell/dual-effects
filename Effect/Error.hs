
module Effect.Error where

import Control.Monad.Catch

import Core
import Effect.Final
import Effect.Embed

data Error m a where
  Raise  :: Exception e => e -> Error m a
  Handle :: Exception e => m a -> (e -> m a) -> Error m a

instance Effect Error where
  weave f (Raise e)       = Raise e
  weave f (Handle ma ema) = Handle (f ma) (f . ema)

raise :: forall e fs a. (Exception e, Member Error fs) => e -> Eff fs a
raise e = send (Raise e)

handle
  :: forall e fs a
  .  (Member Error fs, Exception e)
  => Eff fs a
  -> (e -> Eff fs a)
  -> Eff fs a
handle ma ema = send (Handle ma ema)

errorViaIO
  :: forall m fs
  .  (MonadCatch m, Members '[Embed m, Final m] fs, Diag fs fs)
  => Eff (Error : fs)
  ~> Eff          fs
errorViaIO = interpret \case
  Raise  e      -> embed @m (throwM e)
  Handle ma ema -> do
    nma  <- final  @m ma
    nema <- final1 @m ema
    embed $ catch nma nema
