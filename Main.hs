
module Main where

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
  :: forall m fs
  .  ( Members [Store String, Trace, Env Int, Error, Embed IO] fs
     , Diag fs fs
     )
  => String
  -> Eff fs Int
someEffect str = do
    override @Int (subtract 2) do
      change \s -> str <> s
      e <- env
      track $ "E is " ++ show e
      if e <= 0
      then do
        throwM (Err 1.0)
      else do
        return e
  `catch` \(Err d) -> do
    liftIO $ putStrLn "hehe"
    return 42

main = do
  ref <- newIORef "bar"
  x <- flip runReaderT (And ref (And (2 :: Int) None))
    $ runM
    $ embedToFinal @M
    $ embedViaNat @IO @M liftIO
    $ asReader                 @(Product [IORef String, Int]) @M
    $ mergeEnv @(IORef String) @[IORef String, Int]
    $ mergeEnv @Int            @[IORef String, Int]
    $ errorViaIO @M
    $ storeViaRIO @String @M
    $ debugTrace
    $ someEffect "foo"
  print x
  print =<< readIORef ref