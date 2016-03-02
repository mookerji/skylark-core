-- |
-- Module:      Network.Skylark.Core.Providers.StatsD
-- Copyright:   (c) 2015 Mark Fine
-- License:     BSD3
-- Maintainer:  Mark Fine <mark@swift-nav.com>
--
-- StatGrab providers module for Skylark Core.

module Network.Skylark.Core.Providers.StatGrab
  ( module System.Statgrab
  , sampleStats
  ) where

import Network.Skylark.Core.Prelude
import Network.Skylark.Core.Types
import System.Statgrab

host :: Stats Host
host = snapshot

memory :: Stats Memory
memory = snapshot

load :: Stats Load
load = snapshot

diskIO :: Stats [DiskIO]
diskIO = snapshots

networkIO :: Stats [NetworkIO]
networkIO = snapshots

fileSystem :: Stats [FileSystem]
fileSystem = snapshots

sampleStats ::  Stats SystemStat
sampleStats = (,,,,,) <$> host <*> memory <*> load <*> diskIO <*> networkIO <*> fileSystem
