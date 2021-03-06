module Devel.Git where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Conduit.Process
import qualified Data.Conduit.Text as CT
import           Data.List
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import           Filesystem.Path (FilePath)
import qualified Filesystem.Path.CurrentOS as FP
import           Prelude hiding (FilePath)

-- | Get files in the Git repo.
getGitFiles :: (MonadThrow m,MonadIO m)
            => m (Set FilePath)
getGitFiles =
  do (_,ps) <- sourceProcessWithConsumer
                 (proc "git" (["ls-files", "-o", "--exclude-standard", "-c"] ++ ignored))
                 (CT.decodeUtf8 $= CT.lines $= CL.filter (not . T.isSuffixOf (T.pack ".hs")) $= CL.consume)
     return (S.fromList
               (map (FP.decodeString . T.unpack)
                    (T.words (T.unlines ps))))
  where ignored = "-x" : intersperse "-x" ignoredFiles
        ignoredFiles = ["*.png", "*.psd", "*.tar", "*.tar.gz", "session.*", "*.db", "*.sqlite*", "*.hs"]
