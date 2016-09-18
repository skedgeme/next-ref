{-# LANGUAGE LambdaCase, RecordWildCards, BangPatterns #-}
{-| This package contains a concurrency primitive which can be used to limit an
    expensive consumer from running unnecessarily. Crucially the consumer must
    be able to tolerate missing some updates.

    'NextRef' provides non-blocking writes, blocking reads, and non-blocking 
    reads.

    The blocking read interface ('takeNextRef') will not necessarily present 
    all values. 

    Additionally the 'NextRef' can be 'closed'. This is useful to graceful
    shutdown the consumer when the producer closes the 'NextRef'
    
-}
module Control.Concurrent.NextRef 
  ( NextRef 
  , newNextRef
  , takeNextRef
  , readLast
  , writeNextRef
  , modifyNextRef 
  , close
  , open
  , status
  , Status (..)
  ) where
import Control.Concurrent.STM
import Data.IORef

-- | Status is used to prevent future reads. When the status is 'Closed'
--   'takeNextRef' will always return 'Nothing'. When the status is 
--   open it will return Just. This is based off of the design of 'TMQueue'
--   from the 'stm-chans' package
data Status = Open | Closed
  deriving (Show, Eq, Ord, Read, Enum, Bounded)

-- | A concurrency primitive for a slow consumer that can tolerate
--   missing some updates.
data NextRef a = NextRef  
  { nrAccum     :: IORef a
  , nrNextValue :: TMVar a
  , nrStatus    :: TVar  Status 
  }

-- | Create a 'NextVar'
newNextRef :: a -> IO (NextRef a)
newNextRef x = NextRef <$> newIORef x <*> newTMVarIO x <*> newTVarIO Open

-- | Block until the next value is available. If the 'NextVar' is 
--   closed it returns 'Nothing' immediantly. 
takeNextRef :: NextRef a -> IO (Maybe a)
takeNextRef NextRef {..} = atomically $ readTVar nrStatus >>= \case
  Closed -> return Nothing
  Open   -> Just <$> takeTMVar nrNextValue

update :: NextRef a -> a -> IO ()
update NextRef {..} !new = atomically $ readTVar nrStatus >>= \case
  Closed -> return ()
  Open   -> do
    tryTakeTMVar nrNextValue
    putTMVar     nrNextValue new

-- | Read the most recent value. Non-blocking
readLast :: NextRef a -> IO a
readLast NextRef {..} = readIORef nrAccum

tupleResult :: (a, b) -> (a, (a, b))
tupleResult (x, y) = (x, (x, y))

-- | Write a new value. Never blocks.
writeNextRef :: NextRef a -> a -> IO ()
writeNextRef nv@(NextRef {..}) newValue = do 
  writeIORef nrAccum newValue
  update nv newValue

-- | Apply a function to current value to produce the next value and return 
--   a result. 
modifyNextRef :: NextRef a -> (a -> (a, b)) -> IO b
modifyNextRef nv@(NextRef {..}) f = do
  (!newValue, !result) <- atomicModifyIORef' nrAccum $ tupleResult . f
  update nv newValue
  return result

-- | Modify the status of the 'NextRef' to 'Closed'. All future reads
--   using 'takeNextRef' will result a 'Nothing'. 'readLast' is unaffected.
close :: NextRef a -> IO ()
close NextRef {..} = atomically $ writeTVar nrStatus Closed

-- | Modify the status of the 'NextRef' to 'Closed'. All future reads
--   using 'takeNextRef' will return a 'Just'. 'readLast' is unaffected.
open :: NextRef a -> IO ()
open NextRef {..} = atomically $ writeTVar nrStatus Open

-- | Get the current status of the 'NextRef'
status :: NextRef a -> IO Status
status = atomically . readTVar . nrStatus