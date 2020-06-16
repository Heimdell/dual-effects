
module Effect.Trace
  ( -- * Interface
    Trace
  , track

    -- * Implementation
  , writeTrace
  , debugTrace

    -- * Re-exporting core
  , module Core
  )
  where

import Data.String

import Core
import Effect.Write

import Debug.Trace

data Trace (m :: * -> *) a where
  Trace :: String -> Trace m ()
  deriving anyclass Effect

track :: Member Trace fs => String -> Eff fs ()
track s = send (Trace s)

writeTrace
  :: (Member (Write [String]) fs, Diag fs fs)
  => Eff (Trace : fs)
  ~> Eff fs
writeTrace = interpret \case
  Trace s -> say [s]

debugTrace
  :: Diag fs fs
  => Eff (Trace : fs)
  ~> Eff fs
debugTrace = interpret \case
  Trace s -> traceM s