module TestImport.Compilation where

import Crypto.Hash.MD5 qualified as MD5
import Data.ByteString qualified as B
import Data.Foldable (for_)
import Data.Function ((&))
import Data.IntMap (IntMap)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Path (AbsPath, RelPath)
import Data.Path qualified as Path
import Data.Pos (Pos)
import Data.Rope qualified as Rope
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T.Encoding
import Data.Text.IO qualified as T.IO
import Data.Traversable (for)
import StaticLS.Logger qualified as Logger
import StaticLS.Server qualified as Server
import StaticLS.StaticEnv.Options qualified as StaticEnv.Options
import StaticLS.StaticLsEnv (StaticLsM)
import StaticLS.StaticLsEnv qualified as StaticLsEnv
import System.Directory qualified as Dir
import System.FilePath ((</>))
import System.Process.Typed qualified as Process
import TestImport.HieDb qualified
import TestImport.Placeholder qualified as Placeholder

setupWithoutCompilation ::
  [(RelPath, Text)] ->
  (AbsPath -> Map AbsPath (Text, IntMap Pos) -> StaticLsM a) ->
  IO a
setupWithoutCompilation sourceFiles act = do
  dir <- Path.filePathToAbs "."
  sourceFiles <- pure $ Map.fromList $ map (\(p, t) -> (p, t)) sourceFiles
  ppSources <- traverse Placeholder.parseM sourceFiles
  absSources <- for (Map.toList ppSources) \(path, t@(contents, _)) -> do
    let absPath = dir Path.</> path
    T.IO.writeFile (Path.toFilePath absPath) contents
    pure (absPath, t)
  staticEnv <- StaticLsEnv.initStaticLsEnv dir StaticEnv.Options.defaultStaticEnvOptions Logger.noOpLogger
  res <- StaticLsEnv.runStaticLsM staticEnv do
    for_ absSources \(absPath, (contents, _)) -> do
      Server.updateFileState absPath (Rope.fromText contents)
    act dir (Map.fromList absSources)
  pure res

setupCompilation ::
  Text ->
  [(RelPath, Text)] ->
  (AbsPath -> Map AbsPath (Text, IntMap Pos) -> StaticLsM a) ->
  IO a
setupCompilation prefix sourceFiles act = do
  let stringHash = prefix <> "\n\n" <> T.concat (map (\(p, t) -> T.pack (Path.toFilePath p) <> "\n\n" <> t <> "\n\n") sourceFiles)
  let md5Hash = MD5.hash (T.Encoding.encodeUtf8 stringHash)
  let md5Path = T.unpack $ "test_" <> T.concat ((T.pack . show) <$> B.unpack md5Hash)
  let dir = (".test_builds" </> md5Path)
  Dir.createDirectoryIfMissing True dir
  sourceFiles <- pure $ Map.fromList $ map (\(p, t) -> (p, t)) sourceFiles
  dir <- Path.filePathToAbs dir
  ppSources <- traverse Placeholder.parseM sourceFiles
  T.IO.writeFile (Path.toFilePath dir </> ".string_hash") stringHash
  absSources <- for (Map.toList ppSources) \(path, t@(contents, _)) -> do
    let absPath = dir Path.</> path
    T.IO.writeFile (Path.toFilePath absPath) contents
    pure (absPath, t)
  let paths = map fst absSources
  let args =
        (map Path.toFilePath paths)
          ++ [ "-fwrite-ide-info"
             , "-fdefer-type-errors"
             , "-hiedir"
             , ".hiefiles"
             , "-hidir"
             , ".hifiles"
             ]
  let proc = Process.proc "cabal" (["exec", "ghc", "--"] ++ args) & Process.setWorkingDir (Path.toFilePath dir)
  Process.runProcess_ proc
  TestImport.HieDb.indexHieFilesIn
    (Path.toFilePath dir </> ".hiefiles")
    (Path.toFilePath dir </> ".hiedb")
    (Path.toFilePath dir)
  staticEnv <- StaticLsEnv.initStaticLsEnv dir StaticEnv.Options.defaultStaticEnvOptions Logger.noOpLogger
  res <- StaticLsEnv.runStaticLsM staticEnv do
    for_ absSources \(absPath, (contents, _)) -> do
      Server.updateFileState absPath (Rope.fromText contents)
    act dir (Map.fromList absSources)
  pure res