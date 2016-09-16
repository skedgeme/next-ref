{-# LANGUAGE LambdaCase, RecordWildCards, BangPatterns #-}
module Control.Concurrent.NextRef 
  ( NextRef 
  , newNextRef
  , takeNextRef
  , readLast
  , writeNextRef
  , modifyNextRef 
  , close
  , open
  ) where
import Control.Concurrent.STM
import Data.IORef

data Status = Open | Closed

data NextRef a = NextRef  
  { accum     :: IORef a
  , nextValue :: TMVar a
  , status    :: TVar  Status 
  }

newNextRef :: a -> IO (NextRef a)
newNextRef x = NextRef <$> newIORef x <*> newTMVarIO x <*> newTVarIO Open

takeNextRef :: NextRef a -> IO (Maybe a)
takeNextRef NextRef {..} = atomically $ readTVar status >>= \case
  Closed -> return Nothing
  Open   -> Just <$> takeTMVar nextValue
  
update :: NextRef a -> a -> IO ()
update NextRef {..} !new = atomically $ readTVar status >>= \case
  Closed -> return ()
  Open   -> do
    tryTakeTMVar nextValue
    putTMVar     nextValue new
     
readLast :: NextRef a -> IO a
readLast NextRef {..} = readIORef accum

tupleResult :: (a, b) -> (a, (a, b))
tupleResult (x, y) = (x, (x, y))

writeNextRef :: NextRef a -> a -> IO ()
writeNextRef nv@(NextRef {..}) newValue = do 
  writeIORef accum newValue
  update nv newValue

modifyNextRef :: NextRef a -> (a -> (a, b)) -> IO b
modifyNextRef nv@(NextRef {..}) f = do
  (!newValue, !result) <- atomicModifyIORef' accum $ tupleResult . f
  update nv newValue
  return result
  
close :: NextRef a -> IO ()
close NextRef {..} = atomically $ writeTVar status Closed

open :: NextRef a -> IO ()
open NextRef {..} = atomically $ writeTVar status Open