
{- | `Trace.traceM`, but embedded as an `Effect`.
-}
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
import Data.Kind (Type)

import Core
import Effect.Writer

import qualified Debug.Trace as Trace

-- | Ability to write tracing messages.
data Trace (m :: Type -> Type) a where
  Trace :: String -> Trace m ()
  deriving anyclass Effect

-- | Write a tracing message.
trace :: Members '[Trace] fs => String -> Eff fs ()
trace s = send (Trace s)

-- | Implement via `Writer`.
writeTrace
  :: Members '[Writer [String]] fs
  => Eff (Trace : fs)
  ~> Eff fs
writeTrace = plug \case
  Trace s -> tell [s]

-- | Implement via `Trace.traceM`.
debugTrace
  :: Members '[] fs
  => Eff (Trace : fs)
  ~> Eff fs
debugTrace = plug \case
  Trace s -> Trace.traceM s
