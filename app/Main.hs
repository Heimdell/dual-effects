
import Data.IORef

import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Catch
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

type M = ReaderT (Product [IORef String, Int]) IO

someEffect
  :: forall m fs
  .  ( Members [State String, Trace, Reader Int, Except, Lift IO] fs
     , Diag fs fs
     )
  => String
  -> Eff fs Int
someEffect str = do
    local @Int (subtract 2) do
      change \s -> str <> s
      e <- ask
      trace $ "E is " ++ show e
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
    $ liftToFinal   @M
    $ liftViaNat    @IO @M liftIO
    $ asReader      @(Product [IORef String, Int]) @M
    $ mergeEnv      @(IORef String) @[IORef String, Int]
    $ mergeEnv      @Int            @[IORef String, Int]
    $ errorViaCatch @M
    $ storeViaRIO   @String @M
    $ debugTrace
    $ someEffect "foo"
  print x
  print =<< readIORef ref

-- countDown :: Member (State Int) fs => Eff fs Int
-- countDown = do
--   x <- retrieve
--   if x <= 0
--   then do
--     return x
--   else do
--     store (x - 1)
--     countDown

-- countDown' :: MonadState Int m => m Int
-- countDown' = do
--   x <- get
--   if x <= 0
--   then do
--     return x
--   else do
--     put (x - 1)
--     countDown'

-- main = do
--   print
--     $ flip runState (10000000 :: Int)
--     $ countDown'
--     -- $ runM
--     -- $ embedToFinal @(State Int)
--     -- $ asState @Int @(State Int)
--     -- $ countDown