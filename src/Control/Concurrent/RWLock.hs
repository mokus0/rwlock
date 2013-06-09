{-# LANGUAGE DeriveDataTypeable #-}
-- |A simple implementation of a multiple-reader / single-writer lock, using
-- "Control.Concurrent.STM".
module Control.Concurrent.RWLock
    ( RWLock
    , RWLockState(..)
    
    , newRWLock             , newRWLockIO
    , readRWLock            , readRWLockIO
    
    , tryTakeReadLock       , tryPutReadLock
    , takeReadLock          , putReadLock
    , tryTakeReadLockIO     , tryPutReadLockIO
    , takeReadLockIO        , putReadLockIO
    
    , withReadLock
    
    , tryTakeWriteLock      , tryPutWriteLock
    , takeWriteLock         , putWriteLock
    , tryTakeWriteLockIO    , tryPutWriteLockIO
    , takeWriteLockIO       , putWriteLockIO
    
    , withWriteLock
    ) where

import Control.Concurrent.STM
import Control.Monad.Loops.STM
import Control.Exception

import Data.Generics (Data, Typeable)

newtype RWLock = Lock { unLock :: TVar RWLockState }

-- |A type representing the state of a lock: available, in use by a certain
-- number of readers, or in use by a writer.
data RWLockState
    = Open
    | Readers Int
    | Writing
    deriving (Eq, Show, Data, Typeable)

atomicModifyLock f (Lock ref) = do
    x <- readTVar ref
    let (y,z) = f x
    writeTVar ref y
    return z

newRWLock :: STM RWLock
newRWLock = fmap Lock (newTVar Open)

newRWLockIO :: IO RWLock
newRWLockIO = fmap Lock (newTVarIO Open)

readRWLock :: RWLock -> STM RWLockState
readRWLock = readTVar . unLock

readRWLockIO :: RWLock -> IO RWLockState
readRWLockIO = atomically . readRWLock

addReader Open        = (Readers 1,      True)
addReader (Readers n) = (Readers $! n+1, True)
addReader other       = (other,          False)

delReader (Readers 1)     = (Open,      True)
delReader (Readers (n+1)) = (Readers n, True)
delReader other           = (other,     False)

tryTakeReadLock, tryPutReadLock :: RWLock -> STM Bool
tryTakeReadLock = atomicModifyLock addReader
tryPutReadLock  = atomicModifyLock delReader

tryTakeReadLockIO, tryPutReadLockIO :: RWLock -> IO Bool
tryTakeReadLockIO = atomically . tryTakeReadLock
tryPutReadLockIO  = atomically . tryPutReadLock

takeReadLock, putReadLock :: RWLock -> STM ()
takeReadLock = waitForTrue . tryTakeReadLock
putReadLock  = waitForTrue . tryPutReadLock

takeReadLockIO, putReadLockIO :: RWLock -> IO ()
takeReadLockIO = atomically . takeReadLock
putReadLockIO  = atomically . putReadLock

-- |Acquire a lock in read mode, try to execute some action, and release the lock.
withReadLock :: RWLock -> IO a -> IO a
withReadLock l action = bracket_ (takeReadLockIO l) (putReadLockIO l) action

addWriter Open  = (Writing, True)
addWriter other = (other,   False)

delWriter Writing = (Open,  True)
delWriter other   = (other, False)

tryTakeWriteLock, tryPutWriteLock :: RWLock -> STM Bool
tryTakeWriteLock = atomicModifyLock addWriter
tryPutWriteLock  = atomicModifyLock delWriter

tryTakeWriteLockIO, tryPutWriteLockIO :: RWLock -> IO Bool
tryTakeWriteLockIO = atomically . tryTakeWriteLock
tryPutWriteLockIO  = atomically . tryPutWriteLock

takeWriteLock, putWriteLock :: RWLock -> STM ()
takeWriteLock = waitForTrue . tryTakeWriteLock
putWriteLock  = waitForTrue . tryPutWriteLock

takeWriteLockIO, putWriteLockIO :: RWLock -> IO ()
takeWriteLockIO = atomically . takeWriteLock
putWriteLockIO  = atomically . putWriteLock

-- |Acquire a lock in write mode, try to execute some action, and release the lock.
withWriteLock :: RWLock -> IO a -> IO a
withWriteLock l action = bracket_ (takeWriteLockIO l) (putWriteLockIO l) action
