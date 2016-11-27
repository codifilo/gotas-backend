module Cache (CacheState
              , RetrieveFun
              , newEmptyCacheState
              , cleanOldValues
              , readValue) where

import Control.Concurrent
import Control.Concurrent.MVar
import qualified Data.Map as Map
import Data.Time.Clock
import Control.Monad

data CachedValue a = CachedValue {time :: UTCTime, val :: Maybe a}
type CachedValueState a = MVar (CachedValue a)
type Cache k a = Map.Map k (CachedValueState a)
type CacheState k a = MVar (Cache k a)
type RetrieveFun k a = k -> IO (Maybe a)

newEmptyCacheState :: IO (CacheState k a)
newEmptyCacheState = newMVar Map.empty

cleanOldValues :: Ord k => NominalDiffTime -> CacheState k a -> IO ()
cleanOldValues expiry cacheState = do
  cache <- takeMVar cacheState
  now <- getCurrentTime
  filteredCache <- filterValues (\cvs -> do
    result <- tryReadMVar cvs
    case result of
      Nothing -> return True
      Just (CachedValue t _) -> do
        let age = now `diffUTCTime` t
        let isValid = age <= expiry
        unless isValid $ putStrLn $ "Removing expired value: age=" ++ show age
        return isValid
    ) cache
  putMVar cacheState filteredCache

filterValues ::(Ord k, Monad m) => (a -> m Bool) -> Map.Map k a -> m (Map.Map k a)
filterValues pM = Map.foldrWithKey (\k a acc ->
  pM a >>= (\p -> if p
                    then Map.insert k a <$> acc
                    else acc)) (return Map.empty)

readValue :: Ord k => CacheState k a -> k -> RetrieveFun k a -> IO (Maybe a)
readValue cacheState key retrieve = do
  valMaybe <- readValueNonBlockingCache cacheState key
  case valMaybe of
    Just val -> return $ Just val
    Nothing -> readValueBlockingCache cacheState key retrieve

readValueBlockingCache :: Ord k => CacheState k a -> k -> (k -> IO (Maybe a)) -> IO (Maybe a)
readValueBlockingCache cacheState key retrieve = do
  cache <- takeMVar cacheState
  case Map.lookup key cache of
    Nothing -> do
      valueState <- newEmptyMVar
      let updatedCache = Map.insert key valueState cache
      putMVar cacheState updatedCache
      val <- retrieve key
      time <- getCurrentTime
      putMVar valueState (CachedValue time val)
      return val
    Just valueState -> do
      putMVar cacheState cache
      CachedValue time val <- readMVar valueState
      return val

readValueNonBlockingCache :: Ord k => CacheState k a -> k -> IO (Maybe a)
readValueNonBlockingCache cacheState key = do
  cacheMaybe <- tryReadMVar cacheState
  case cacheMaybe >>= Map.lookup key of
    Nothing -> return Nothing
    Just cachedValueState -> do
      (CachedValue time val) <- readMVar cachedValueState
      return val
