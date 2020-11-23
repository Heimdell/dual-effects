
import Data.IORef

import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Catch hiding (finally)
import Control.Monad.IO.Class

import Effect.Final
import Effect.Lift
import Effect.State
import Effect.Reader
import Effect.Except
import Effect.Fixpoint
import Effect.NonDet
import Effect.Writer
import Effect.Resource
import Effect.Trace
import Product

----

data Err = Err Double
  deriving stock    Show
  deriving anyclass Exception

type M = ReaderT (Product [IORef String, Integer]) IO

someEffect
  :: forall fs
  .  Members [State String, Trace, Reader Integer, Except, Resource, Lift IO] fs
  => String
  -> Eff fs Integer
someEffect str = do
    local (subtract 2) do
        modify (str <>)
        e <- ask
        trace $ "E is " ++ show e
        if e <= 0
        then do
          throwM (Err 1.0)
        else do
          return e
      `finally` do
        trace "finally"
  `catch` \(Err d) -> do
    liftIO $ putStrLn ("catched " ++ show d)
    return 42

main = do
  ref <- newIORef "bar"
  x <- flip runReaderT (ref :> (2 :: Integer) :> Nil)
    $ runM
    $ liftToFinal   @M
    $ liftViaNat    @IO @M liftIO
    $ asMask        @M
    $ asReader      @(Product [IORef String, Integer]) @M
    $ mergeEnv      @(IORef String) @[IORef String, Integer]
    $ mergeEnv      @Integer        @[IORef String, Integer]
    $ errorViaCatch @M
    $ storeViaRIO   @String @M
    $ debugTrace
    $ someEffect "foo"
  print x
  print =<< readIORef ref
