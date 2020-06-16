
module Effect.Trace
  ( -- * Interface
    Trace
  , trace

    -- * Implementation
  , writeTrace
  , debugTrace

    -- * Re-exporting core
  , module Core
  )
  where

import Data.String

import Core
import Effect.Writer

import qualified Debug.Trace as Trace

data Trace (m :: * -> *) a where
  Trace :: String -> Trace m ()
  deriving anyclass Effect

trace :: Member Trace fs => String -> Eff fs ()
trace s = send (Trace s)

writeTrace
  :: (Member (Writer [String]) fs, Diag fs fs)
  => Eff (Trace : fs)
  ~> Eff fs
writeTrace = interpret \case
  Trace s -> tell [s]

debugTrace
  :: Diag fs fs
  => Eff (Trace : fs)
  ~> Eff fs
debugTrace = interpret \case
  Trace s -> Trace.traceM s