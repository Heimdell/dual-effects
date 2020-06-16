
import Data.IORef

import Control.Monad.Reader
import Control.Monad.Catch hiding (handle)
import Control.Monad.Fix

import Core
import Effect.Final
import Effect.Embed
import Effect.Store
import Effect.Env
import Effect.Error
import Effect.Fixpoint
import Effect.NonDet
import Effect.Write
import Effect.Resource
import Effect.Trace
import Product

----

data Err = Err Double
  deriving stock    Show
  deriving anyclass Exception

type M = ReaderT (Product [IORef String, Int]) IO

someEffect
  :: Members [Store String, Trace, Env Int, Error] fs
  => String
  -> Eff fs Int
someEffect str = do
    override @Int (subtract 2) do
      change \s -> str <> s
      e <- env
      track $ "E is " ++ show e
      if e <= 0
      then do
        raise (Err 1.0)
      else do
        return e
  `handle` \(Err d) -> do
    return 42

main = do
  ref <- newIORef "bar"
  x <- flip runReaderT (And ref (And (2 :: Int) None))
    $ runM
    $ embedToFinal @M
    $ asReader                 @(Product [IORef String, Int]) @M
    $ mergeEnv @(IORef String) @[IORef String, Int]
    $ mergeEnv @Int            @[IORef String, Int]
    $ errorViaIO @M
    $ storeViaRIO @String @M
    $ debugTrace
    $ someEffect "foo"
  print x
  print =<< readIORef ref