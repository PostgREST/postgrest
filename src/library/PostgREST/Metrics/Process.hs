{-# LANGUAGE CPP #-}

-- |
-- Module      : PostgREST.Metrics.Process
-- Description : Metrics of the PostgREST process.
module PostgREST.Metrics.Process
  ( fdSamples
  )
where

import Prometheus
import Protolude
import System.Directory (listDirectory)

#ifndef mingw32_HOST_OS
import System.Posix.Resource qualified as Resource
#endif

-- | The samples of the process' file descriptors: process_open_fds and
-- process_max_fds, the soft limit of open files. A sample is left out where
-- it can't be read.
fdSamples :: IO [SampleGroup]
fdSamples = do
  openFds <- countEntries "/proc/self/fd" >>= maybe (countEntries "/dev/fd") (pure . Just)
  maxFds <- openFilesLimit
  pure . catMaybes $
    [ gaugeGroup (Info "process_open_fds" "Number of open file descriptors") <$> openFds
    , gaugeGroup (Info "process_max_fds" "Maximum number of open file descriptors") <$> maxFds
    ]
  where
    countEntries dir = rightToMaybe <$> try @IOException (toInteger . length <$> listDirectory dir)
    gaugeGroup info n = SampleGroup info GaugeType [Sample (metricName info) mempty (show n)]

openFilesLimit :: IO (Maybe Integer)
#ifndef mingw32_HOST_OS
openFilesLimit =
  Resource.getResourceLimit Resource.ResourceOpenFiles <&> \limits -> case Resource.softLimit limits of
    Resource.ResourceLimit n -> Just n
    _ -> Nothing
#else
openFilesLimit = pure Nothing
#endif
