-- |
-- Module:      Network.Skylark.Core.Retries
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Mark Fine <mark@swift-nav.com>
--
-- Retries module for Skylark Core. Based on retry package.

module Network.Skylark.Core.Retries
  ( retry
  , recover
  , recoverAll
  , constant
  , fixedDelay
  , exponentialBackoff
  , jitter
  , maxCount
  , maxDelay
  , maxTotal
  , fullJitter
  , equalJitter
  , decorrelatedJitter
  ) where

import Control.Concurrent
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Random
import Data.Default
import Network.Skylark.Core.Prelude hiding (catch)
import Network.Skylark.Core.Types

-- | Update retry state, setting delay from policy and incrementing count.
--
update :: MonadIO m => RetryPolicy -> RetryState -> m RetryState
update policy state = do
  delay <- liftIO $ runRetryPolicy policy state
  return $ state &
    rsCount +~ 1 &
    rsDelay .~ delay &
    rsTotal +~ fromMaybe 0 delay

-- | Apply retry state, update and sleep if necessary.
--
apply :: MonadIO m => RetryPolicy -> RetryState -> m RetryState
apply policy state = do
  state' <- update policy state
  maybe (return ()) (liftIO . threadDelay . fromIntegral) $ state' ^. rsDelay
  return state'

-- | Retry an action with a policy and a check routine.
--
retry :: MonadIO m => RetryPolicy -> (a -> m Bool) -> m a -> m a
retry policy check action =
  loop def where
    loop state = do
      result <- action
      again <- check result
      state' <- apply policy state
      if not again || isNothing (state' ^. rsDelay) then return result else loop state'

-- | Recover exceptions from an action with a policy and a check exception routine.
--
recover :: (MonadIO m, MonadCatch m, Exception e) => RetryPolicy -> (e -> m Bool) -> m a -> m a
recover policy check action =
  loop def where
    loop state =
      catch action $ \e -> do
        again <- maybe (throwM e) check (fromException e)
        state' <- apply policy state
        if not again || isNothing (state' ^. rsDelay) then throwM e else loop state'

-- | Recover all exceptions.
--
recoverAll :: (MonadIO m, MonadCatch m) => RetryPolicy -> (SomeException -> m Bool) -> m a -> m a
recoverAll = recover

-- | Constant policy.
--
constant :: RetryPolicy
constant = RetryPolicy $ \state ->
  return $ state ^. rsDelay

-- | Fixed delay policy.
--
fixedDelay :: Word -> RetryPolicy
fixedDelay delay = RetryPolicy $ \_state ->
  return $ Just delay

-- | Exponential backoff policy.
--
exponentialBackoff :: Word -> RetryPolicy
exponentialBackoff base = RetryPolicy $ \state ->
  return $ Just $ base * 2 ^ (state ^. rsCount)

-- | Jitter delay policy.
--
jitter :: RetryPolicy -> RetryPolicy
jitter policy = RetryPolicy $ \state -> do
  state' <- update policy state
  maybe' (state' ^. rsDelay) (return Nothing) $ \delay -> do
    delay' <- getRandomR (delay - div delay 10, delay + div delay 10)
    return $ Just delay'

-- | Max count policy.
--
maxCount :: Word -> RetryPolicy -> RetryPolicy
maxCount count policy = RetryPolicy $ \state -> do
  state' <- update policy state
  return $ if state' ^. rsCount >= count then Nothing else state' ^. rsDelay

-- | Max delay policy.
--
maxDelay :: Word -> RetryPolicy -> RetryPolicy
maxDelay delay policy = RetryPolicy $ \state -> do
  state' <- update policy state
  return $ min delay <$> state' ^. rsDelay

-- | Max total delay policy.
--
maxTotal :: Word -> RetryPolicy -> RetryPolicy
maxTotal total policy = RetryPolicy $ \state -> do
  state' <- update policy state
  return $ if state' ^. rsTotal >= total then Nothing else state' ^. rsDelay

-- | Full Jitter policy.
--
-- @http:\/\/www.awsarchitectureblog.com\/2015\/03\/backoff.html@
--
-- sleep = random_between(0, min(cap, base * 2 ** attempt))
--
fullJitter :: Word -> RetryPolicy
fullJitter base = RetryPolicy $ \state -> do
  delay <- getRandomR (0, base * 2 ^ (state ^. rsCount))
  return $ Just delay

-- | Equal Jitter policy.
--
-- @http:\/\/www.awsarchitectureblog.com\/2015\/03\/backoff.html@
--
-- temp = min(cap, base * 2 ** attempt)
-- sleep = temp / 2 + random_between(0, temp / 2)
--
equalJitter :: Word -> RetryPolicy
equalJitter base = RetryPolicy $ \state -> do
  let temp = base * 2 ^ (state ^. rsCount)
  delay <- getRandomR (0, div temp 2)
  return $ Just $ div temp 2 + delay

-- | Decorrelated Jitter policy.
--
-- @http:\/\/www.awsarchitectureblog.com\/2015\/03\/backoff.html@
--
-- sleep = min(cap, random_between(base, sleep * 3))
--
decorrelatedJitter :: Word -> RetryPolicy
decorrelatedJitter base = RetryPolicy $ \state -> do
  delay' <- getRandomR (base, max base (fromMaybe base (state ^. rsDelay)) * 3)
  return $ Just delay'
