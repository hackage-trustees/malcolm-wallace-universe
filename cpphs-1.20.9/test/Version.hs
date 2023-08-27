module Version
  ( version
  ) where

import Data.List ( intercalate )
import Data.Version ( Version(versionBranch) )

import qualified Paths_cpphs as Paths

-- | The version of Agda.

version :: String
version = intercalate "." $ map show $
            versionBranch Paths.version

