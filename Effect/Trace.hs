
module Effect.Trace where

import Data.String

import Core
import Effect.Write

data Trace (m :: * -> *) a where
  Trace :: String -> Trace m a

instance Effect Trace where
  weave _ (Trace s) = Trace s

writeTrace
  :: (Member (Write [String]) fs, Diag fs fs)
  => Eff (Trace : fs)
  ~> Eff fs
writeTrace = interpret \case
  Trace s -> say [s]