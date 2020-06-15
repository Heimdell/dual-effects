
import Data.IORef

import Control.Monad.Reader
import Control.Monad.Catch

import Core
import Effect.Final
import Effect.Embed
import Effect.Store
import Effect.Env
import Effect.Error
import Product

-- State


-- Reader

-- Exceptions

-- Example

data Err = Err Double
  deriving (Show)

instance Exception Err

type M = ReaderT (Product [IORef String, Int]) IO

someEffect
  :: Members [Store String, Env Int, Error] fs
  => String
  -> Eff fs Int
someEffect str = do
  change (str <>)
  e <- env
  if e <= 0
  then do
    raise (Err 1.0)
  else do
    return e

main = do
  ref <- newIORef "bar"
  x <- flip runReaderT (And ref (And (5 :: Int) None))
    $ runM
    $ embedToFinal @M
    $ asReader                 @(Product [IORef String, Int]) @M
    $ mergeEnv @(IORef String) @[IORef String, Int]
    $ mergeEnv @Int            @[IORef String, Int]
    $ errorViaIO @M
    $ storeViaRIO @String @M
    $ someEffect "foo"
  print x
  print =<< readIORef ref