{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StrictData #-}

module Nonce where

import           Data.Bits
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import qualified Data.Foldable         as Foldable
import           Data.IORef
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as Map
import           Data.Set              (Set)
import qualified Data.Set              as Set
import           Data.Time.Clock
import           Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import           Data.Word
import           Sign

-- How far back we keep nonces
nonce_store_timeout :: NominalDiffTime
nonce_store_timeout = 5 --seconds

-- Nonces older than this are rejected. Has to be < nonce_store_timeout or we
-- can accept nonces that we already "forgot" about. Difference between the two
-- is the grace period we have for handling the nonce check.
nonce_reject_timeout :: NominalDiffTime
nonce_reject_timeout = 4 -- second

type Nonce = Word64

--------------------------------------------------------------------------------
-- Creating Nonces -------------------------------------------------------------
--------------------------------------------------------------------------------

type NoncePool = IORef Nonce

newNoncePool :: IO NoncePool
newNoncePool = do
  -- Start nonce counter using current time to avoid replaying nonces
  start <- getPOSIXTime
    -- Seconds since epoch times takes roughtly 31 bits to store
    -- log_2(50 * 365 * 24 * 60 * 60) ~ 30.6 with 32 being enough for 130 years
    -- so we can take the lower 32 bit for counting
  newIORef $ round start {-seconds -} `shiftL` 32

mkNonce :: NoncePool -> IO Nonce
mkNonce pool = do
  atomicModifyIORef pool (\x -> (x+1,x+1))

--------------------------------------------------------------------------------
-- Checking Nonces -------------------------------------------------------------
--------------------------------------------------------------------------------
-- Instead of a "sliding" window we have two windows in a frame, one going from
-- (split - timout) to split and one going from split to current-time. We always
-- add new nonces to the current window, ensuring that all nonces in the old
-- window are at least (now - split) old. Once split is older than the timeout
-- (and hence all nonces in the old window are older than the timeout) we
-- discard the old window, cycle the new window to become the old frame and
-- start an empty frame as the current frame.
--
-- This ensures that we find duplicate nonces _at least_ as far back as (now -
-- timeout), possible further.
data NonceFrame =
  NonceFrame { nfOldNonces :: Set Nonce -- nonces we got before the split
             , nfSplit :: POSIXTime -- when we split
             , nfCurrentNonces :: Set Nonce -- current frame
             } deriving Show

type Frame = IORef NonceFrame

newFrame :: IO Frame
newFrame = do
  now <- getPOSIXTime
  newIORef (NonceFrame Set.empty now Set.empty)

-- | Cycle frames, check if nonce is present and insert if it is not
updateCheckNonceFrame
  :: POSIXTime -- ^ Current time
  -> NominalDiffTime -- ^ timeout before we can drop nonces
  -> Nonce -- ^ Nonce to handle
  -> NonceFrame -- ^ Frames to look nonce up
  -> (NonceFrame, Bool)
updateCheckNonceFrame now timeout nonce frames'@(NonceFrame _old _split current') =
  let cutoff = now - timeout
  -- Cycle frames if necessary
      frames@(NonceFrame old split current) =
        if split < cutoff
          then NonceFrame current' now Set.empty
          else frames'
      found = Set.member nonce old || Set.member nonce current
  in if found
     -- Found the nonce, reject the message and don't update the frames
     then (frames, False)
     -- Fresh nonce, add to current frame and don't reject message
     else  (NonceFrame old split (Set.insert nonce current), True)

data Verdict = Reject | Accept deriving Show

instance Semigroup Verdict where
  Accept <> Accept = Accept
  _ <> _ = Reject

handleNonce :: Frame -> POSIXTime -> Nonce  -> IO Verdict
handleNonce ref timestamp nonce = do
  now <- getPOSIXTime
  -- timestamp is before timeout
  if nonce_reject_timeout + timestamp < now
    then return Reject
    else do
      accept <- atomicModifyIORef ref
               $ updateCheckNonceFrame now nonce_store_timeout nonce
      return $ if accept then Accept else Reject
