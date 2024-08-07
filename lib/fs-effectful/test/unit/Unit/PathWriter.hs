{-# LANGUAGE QuasiQuotes #-}

module Unit.PathWriter (tests) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Data.IORef
  ( IORef,
    modifyIORef',
    newIORef,
    readIORef,
  )
import Data.List qualified as L
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.Dispatch.Dynamic (reinterpret)
import Effectful.Exception
  ( IOException,
    StringException,
    displayException,
    throwString,
    try,
  )
import Effectful.FileSystem.FileReader.Dynamic (readBinaryFile, runFileReaderDynamicIO)
import Effectful.FileSystem.FileWriter.Dynamic
  ( FileWriterDynamic,
    OsPath,
    runFileWriterDynamicIO,
    writeBinaryFile,
  )
import Effectful.FileSystem.FileWriter.Dynamic qualified as FW
import Effectful.FileSystem.PathReader.Dynamic
  ( PathReaderDynamic,
    doesDirectoryExist,
    doesFileExist,
    runPathReaderDynamicIO,
  )
import Effectful.FileSystem.PathReader.Dynamic qualified as PR
import Effectful.FileSystem.PathWriter.Dynamic
  ( CopyDirConfig (MkCopyDirConfig),
    Overwrite (OverwriteAll, OverwriteDirectories, OverwriteNone),
    PathWriterDynamic
      ( CopyFileWithMetadata,
        CreateDirectory,
        CreateDirectoryIfMissing,
        RemoveDirectory,
        RemoveDirectoryRecursive,
        RemoveFile
      ),
    TargetName (TargetNameDest, TargetNameLiteral, TargetNameSrc),
    copyFileWithMetadata,
    createDirectory,
    createDirectoryIfMissing,
    removeDirectory,
    removeDirectoryRecursive,
    removeFile,
    runPathWriterDynamicIO,
  )
import Effectful.FileSystem.PathWriter.Dynamic qualified as PW
import Effectful.FileSystem.PathWriter.Dynamic qualified as PathWriter
import Effectful.FileSystem.Utils (osp, (</>))
import Effectful.FileSystem.Utils qualified as Utils
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@=?))
import TestUtils qualified as U

tests :: IO OsPath -> TestTree
tests getTmpDir =
  testGroup
    "PathWriter"
    [ copyDirectoryRecursiveTests getTmpDir,
      removeLinkTests getTmpDir,
      copyLinkTests getTmpDir,
      removeExistsTests getTmpDir
    ]

copyDirectoryRecursiveTests :: IO OsPath -> TestTree
copyDirectoryRecursiveTests getTmpDir =
  testGroup
    "copyDirectoryRecursive"
    [ overwriteTests getTmpDir,
      copyDirectoryRecursiveMiscTests getTmpDir
    ]

overwriteTests :: IO OsPath -> TestTree
overwriteTests getTmpDir =
  testGroup
    "Overwrite"
    [ cdrOverwriteNoneTests getTmpDir,
      cdrOverwriteTargetTests getTmpDir,
      cdrOverwriteAllTests getTmpDir
    ]

cdrOverwriteNoneTests :: IO OsPath -> TestTree
cdrOverwriteNoneTests getTmpDir =
  testGroup
    "OverwriteNone"
    [ cdrnFresh getTmpDir,
      cdrnCustomTarget getTmpDir,
      cdrnDestNonExtantFails getTmpDir,
      cdrnOverwriteFails getTmpDir,
      cdrnPartialFails getTmpDir
    ]

cdrnFresh :: IO OsPath -> TestTree
cdrnFresh getTmpDir = testCase desc $ do
  tmpDir <- mkTestPath getTmpDir [osp|cdrnFresh|]
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> [osp|dest|]

  runEffPathWriter $ do
    PathWriter.createDirectoryIfMissing False destDir

    PathWriter.copyDirectoryRecursiveConfig
      (overwriteConfig OverwriteNone)
      srcDir
      destDir

  assertSrcExists tmpDir
  assertDestExists tmpDir
  where
    desc = "Copy to fresh directory succeeds"

cdrnCustomTarget :: IO OsPath -> TestTree
cdrnCustomTarget getTmpDir = testCase desc $ do
  tmpDir <- mkTestPath getTmpDir [osp|cdrnCustomTarget|]
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> [osp|dest|]
      target = [osp|target|]

  runEffPathWriter $ do
    createDirectoryIfMissing False destDir

    PathWriter.copyDirectoryRecursiveConfig
      (MkCopyDirConfig OverwriteNone (TargetNameLiteral target))
      srcDir
      destDir

  assertSrcExists tmpDir
  assertFilesExist $
    (destDir </>)
      <$> [ [osp|target/a/b/c/f1|],
            [osp|target/a/f2|],
            [osp|target/a/b/f3|],
            [osp|target/a/f4|],
            [osp|target/a/f5|],
            [osp|target/a/b/f5|]
          ]
  assertDirsExist $
    (destDir </>)
      <$> [ [osp|target/a/b/c|],
            [osp|target/empty/d|]
          ]
  where
    desc = "Copy with custom directory succeeds"

cdrnDestNonExtantFails :: IO OsPath -> TestTree
cdrnDestNonExtantFails getTmpDir = testCase desc $ do
  tmpDir <- mkTestPath getTmpDir [osp|cdrnDestNonExtantFails|]
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> [osp|dest|]

  -- NOTE: This commented line is why the test fails: no dest dir
  -- createDirectoryIfMissing False destDir

  -- copy files
  result <-
    try $
      runEffPathWriter $
        PathWriter.copyDirectoryRecursiveConfig
          (overwriteConfig OverwriteNone)
          srcDir
          destDir
  resultEx <- case result of
    Right _ -> assertFailure "Expected exception, received none"
    Left (ex :: IOException) -> pure ex

  let exText = displayException resultEx

  assertBool exText (suffix `L.isSuffixOf` exText)

  -- assert original files remain
  assertSrcExists tmpDir

  -- assert files were _not_ copied
  assertDirsDoNotExist [destDir]
  where
    desc = "Copy to non-extant dest fails"
    suffix = "dest: getPathType: does not exist (path does not exist)"

cdrnOverwriteFails :: IO OsPath -> TestTree
cdrnOverwriteFails getTmpDir = testCase desc $ do
  tmpDir <- mkTestPath getTmpDir [osp|cdrnExtantFails|]
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> [osp|dest|]

  runEffPathWriter $ do
    createDirectoryIfMissing False destDir

    -- NOTE: This causes the expected error
    createDirectoryIfMissing False (destDir </> [osp|src|])

  -- copy files
  result <-
    try $
      runEffPathWriter $
        PathWriter.copyDirectoryRecursiveConfig
          (overwriteConfig OverwriteNone)
          srcDir
          destDir
  resultEx <- case result of
    Right _ -> assertFailure "Expected exception, received none"
    Left (ex :: IOException) -> pure ex

  let exText = displayException resultEx

  assertBool exText (suffix `L.isSuffixOf` exText)

  -- assert original files remain
  assertSrcExists tmpDir

  -- assert files were _not_ copied
  assertDirsDoNotExist $
    (destDir </>)
      <$> [ [osp|src/a/|],
            [osp|src/empty|]
          ]
  where
    desc = "Copy to extant dest/<target> fails"
    suffix = "src: copyDirectoryNoOverwrite: already exists (Attempted directory overwrite when CopyDirConfig.overwrite is OverwriteNone)"

cdrnPartialFails :: IO OsPath -> TestTree
cdrnPartialFails getTmpDir = testCase desc $ do
  tmpDir <- mkTestPath getTmpDir [osp|cdrnPartialFails|]
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> [osp|dest|]

  runEffPathWriter $ createDirectoryIfMissing False destDir

  -- copy files
  result <-
    try $
      runPartialDynamicIO $
        PathWriter.copyDirectoryRecursiveConfig
          (overwriteConfig OverwriteNone)
          srcDir
          destDir
  resultEx <- case result of
    Right _ -> assertFailure "Expected exception, received none"
    Left (ex :: StringException) -> pure ex

  let exText = displayException resultEx

  assertBool exText ("Failed copying" `L.isInfixOf` exText)

  -- assert original files remain
  assertSrcExists tmpDir

  -- assert no files left over after partial write
  assertDirsDoNotExist [destDir </> [osp|src|]]
  where
    desc = "Partial failure rolls back changes"

cdrOverwriteTargetTests :: IO OsPath -> TestTree
cdrOverwriteTargetTests getTmpDir =
  testGroup
    "OverwriteDirectories"
    [ cdrtFresh getTmpDir,
      cdrtDestNonExtantFails getTmpDir,
      cdrtOverwriteTargetSucceeds getTmpDir,
      cdrtOverwriteTargetMergeSucceeds getTmpDir,
      cdrtOverwriteTargetMergeFails getTmpDir,
      cdrtOverwriteFileFails getTmpDir,
      cdrtPartialFails getTmpDir,
      cdrtOverwritePartialFails getTmpDir
    ]

cdrtFresh :: IO OsPath -> TestTree
cdrtFresh getTmpDir = testCase desc $ do
  tmpDir <- mkTestPath getTmpDir [osp|cdrtFresh|]
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> [osp|dest|]

  runEffPathWriter $ do
    createDirectoryIfMissing False destDir

    PathWriter.copyDirectoryRecursiveConfig
      (overwriteConfig OverwriteDirectories)
      srcDir
      destDir

  assertSrcExists tmpDir
  assertDestExists tmpDir
  where
    desc = "Copy to fresh directory succeeds"

cdrtDestNonExtantFails :: IO OsPath -> TestTree
cdrtDestNonExtantFails getTmpDir = testCase desc $ do
  tmpDir <- mkTestPath getTmpDir [osp|cdrtDestNonExtantFails|]
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> [osp|dest|]

  -- NOTE: This commented line is why the test fails: no dest dir
  -- createDirectoryIfMissing False destDir

  -- copy files
  result <-
    try $
      runEffPathWriter $
        PathWriter.copyDirectoryRecursiveConfig (overwriteConfig OverwriteDirectories) srcDir destDir
  resultEx <- case result of
    Right _ -> assertFailure "Expected exception, received none"
    Left (ex :: IOException) -> pure ex

  let exText = displayException resultEx

  assertBool exText (suffix `L.isSuffixOf` exText)

  -- assert original files remain
  assertSrcExists tmpDir

  -- assert files were _not_ copied
  assertDirsDoNotExist [destDir]
  where
    desc = "Copy to non-extant dest fails"
    suffix = "dest: getPathType: does not exist (path does not exist)"

cdrtOverwriteTargetSucceeds :: IO OsPath -> TestTree
cdrtOverwriteTargetSucceeds getTmpDir = testCase desc $ do
  tmpDir <- mkTestPath getTmpDir [osp|cdrtOverwriteTargetSucceeds|]
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> [osp|dest|]

  runEffPathWriter $ do
    createDirectoryIfMissing False destDir

    -- NOTE: test that dir already exists and succeeds
    createDirectoryIfMissing False (destDir </> [osp|src|])
    createDirectoryIfMissing False (destDir </> [osp|src/test|])
    writeFiles [(destDir </> [osp|src/test/here|], "cat")]

    -- copy files
    PathWriter.copyDirectoryRecursiveConfig
      (overwriteConfig OverwriteDirectories)
      srcDir
      destDir

  assertSrcExists tmpDir
  assertFilesExist [destDir </> [osp|src/test/here|]]
  assertDestExists tmpDir
  where
    desc = "copy to extant dest/<target> succeeds"

cdrtOverwriteTargetMergeSucceeds :: IO OsPath -> TestTree
cdrtOverwriteTargetMergeSucceeds getTmpDir = testCase desc $ do
  tmpDir <- mkTestPath getTmpDir [osp|cdrtOverwriteTargetMergeSucceeds|]
  let srcDir = tmpDir </> [osp|src|]
      destDir = tmpDir </> [osp|dest|]

  runEffPathWriter $ createDirectoryIfMissing True destDir
  runEffPathWriter $ createDirectoryIfMissing True srcDir

  -- NOTE: test that dir already exists and succeeds
  let d1 = destDir </> [osp|one/|]
      d1Files = (d1 </>) <$> [[osp|f1|], [osp|f2|]]
      d2 = destDir </> [osp|two/|]
      d2Files = (d2 </>) <$> [[osp|f1|], [osp|f2|]]

      s1 = srcDir </> [osp|one/|]
      s1Files = (s1 </>) <$> [[osp|f3|], [osp|f4|]]
      s2 = srcDir </> [osp|two/|]
      s2Files = (s2 </>) <$> [[osp|f3|], [osp|f4|]]

  runEffPathWriter $ do
    createDirectoryIfMissing False d1
    createDirectoryIfMissing False d2
    createDirectoryIfMissing False s1
    createDirectoryIfMissing False s2
    writeFiles $
      map (,"cat") d1Files
        ++ map (,"cat") d2Files
        ++ map (,"cat") s1Files
        ++ map (,"cat") s2Files

    -- copy files
    PathWriter.copyDirectoryRecursiveConfig
      config
      srcDir
      destDir

  -- assert copy correctly merged directories
  assertFilesExist $
    (destDir </>)
      <$> [ [osp|one/f1|],
            [osp|one/f2|],
            [osp|one/f3|],
            [osp|one/f4|],
            [osp|two/f1|],
            [osp|two/f2|],
            [osp|two/f3|],
            [osp|two/f4|]
          ]

  -- src still exists
  assertFilesExist $
    (srcDir </>)
      <$> [ [osp|one/f3|],
            [osp|one/f4|],
            [osp|two/f3|],
            [osp|two/f4|]
          ]
  assertFilesDoNotExist $
    (srcDir </>)
      <$> [ [osp|one/f1|],
            [osp|one/f2|],
            [osp|two/f1|],
            [osp|two/f2|]
          ]
  where
    desc = "copy to extant dest/<target> merges successfully"
    config = MkCopyDirConfig OverwriteDirectories TargetNameDest

cdrtOverwriteTargetMergeFails :: IO OsPath -> TestTree
cdrtOverwriteTargetMergeFails getTmpDir = testCase desc $ do
  tmpDir <- mkTestPath getTmpDir [osp|cdrtOverwriteTargetMergeFails|]
  let srcDir = tmpDir </> [osp|src|]
      destDir = tmpDir </> [osp|dest|]

  runEffPathWriter $ do
    createDirectoryIfMissing True destDir
    createDirectoryIfMissing True srcDir

  -- NOTE: test that dir already exists and succeeds
  let d1 = destDir </> [osp|one|]
      d1Files = (d1 </>) <$> [[osp|f1|], [osp|f2|]]
      d2 = destDir </> [osp|two|]
      -- f3 introduces the collision failure we want
      d2Files = (d2 </>) <$> [[osp|f1|], [osp|f2|], [osp|f3|]]

      s1 = srcDir </> [osp|one|]
      s1Files = (s1 </>) <$> [[osp|f3|], [osp|f4|]]
      s2 = srcDir </> [osp|two|]
      s2Files = (s2 </>) <$> [[osp|f3|], [osp|f4|]]

  runEffPathWriter $ do
    createDirectoryIfMissing False d1
    createDirectoryIfMissing False d2
    createDirectoryIfMissing False s1
    createDirectoryIfMissing False s2
    writeFiles $
      map (,"cat") d1Files
        ++ map (,"cat") d2Files
        ++ map (,"cat") s1Files
        ++ map (,"cat") s2Files

  -- copy files
  result <-
    try $
      runEffPathWriter $
        PathWriter.copyDirectoryRecursiveConfig
          config
          srcDir
          destDir
  resultEx <- case result of
    Right _ -> assertFailure "Expected exception, received none"
    Left (ex :: IOException) -> pure ex

  let exText = displayException resultEx

  assertBool exText (suffix `L.isSuffixOf` exText)

  -- assert dest unchanged from bad copy
  assertFilesExist $
    (destDir </>)
      <$> [ [osp|one/f1|],
            [osp|one/f2|],
            [osp|two/f1|],
            [osp|two/f2|],
            [osp|two/f3|]
          ]

  assertFilesDoNotExist $
    (destDir </>)
      <$> [ [osp|one/f3|],
            [osp|one/f4|],
            [osp|two/f4|]
          ]

  -- src still exists
  assertFilesExist $
    (srcDir </>)
      <$> [ [osp|one/f3|],
            [osp|one/f4|],
            [osp|two/f3|],
            [osp|two/f4|]
          ]
  assertFilesDoNotExist $
    (srcDir </>)
      <$> [ [osp|one/f1|],
            [osp|one/f2|],
            [osp|two/f1|],
            [osp|two/f2|]
          ]
  where
    desc = "copy to extant dest/<target> merge fails"
    config = MkCopyDirConfig OverwriteDirectories TargetNameDest
    suffix = "f3: copyDirectoryOverwrite: already exists (Attempted file overwrite when CopyDirConfig.overwriteFiles is false)"

cdrtOverwriteFileFails :: IO OsPath -> TestTree
cdrtOverwriteFileFails getTmpDir = testCase desc $ do
  tmpDir <- mkTestPath getTmpDir [osp|cdrtOverwriteFileFails|]
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> [osp|dest|]

  runEffPathWriter $ do
    createDirectoryIfMissing True (destDir </> [osp|src/a/b/c|])

    -- NOTE: this line causes it to die
    writeFiles [(destDir </> [osp|src/a/b/c/f1|], "cat")]

  -- copy files
  result <-
    try $
      runEffPathWriter $
        PathWriter.copyDirectoryRecursiveConfig
          (overwriteConfig OverwriteDirectories)
          srcDir
          destDir
  resultEx <- case result of
    Right _ -> assertFailure "Expected exception, received none"
    Left (ex :: IOException) -> pure ex

  let exText = displayException resultEx

  assertBool exText (suffix `L.isSuffixOf` exText)

  -- assert original files remain
  assertSrcExists tmpDir
  assertFilesExist [destDir </> [osp|src/a/b/c/f1|]]
  where
    desc = "copy to extant dest/<target>/file fails"
    suffix = "f1: copyDirectoryOverwrite: already exists (Attempted file overwrite when CopyDirConfig.overwriteFiles is false)"

cdrtPartialFails :: IO OsPath -> TestTree
cdrtPartialFails getTmpDir = testCase desc $ do
  tmpDir <- mkTestPath getTmpDir [osp|cdrtPartialFails|]
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> [osp|dest|]

  runEffPathWriter $ createDirectoryIfMissing False destDir

  -- copy files
  result <-
    try $
      runPartialDynamicIO $
        PathWriter.copyDirectoryRecursiveConfig
          (overwriteConfig OverwriteDirectories)
          srcDir
          destDir
  resultEx <- case result of
    Right _ -> assertFailure "Expected exception, received none"
    Left (ex :: StringException) -> pure ex

  let exText = displayException resultEx

  assertBool exText ("Failed copying" `L.isInfixOf` exText)

  -- assert original files remain
  assertSrcExists tmpDir

  -- assert no files left over after partial write
  assertDirsDoNotExist [destDir </> [osp|src|]]
  where
    desc = "Partial failure rolls back changes"

cdrtOverwritePartialFails :: IO OsPath -> TestTree
cdrtOverwritePartialFails getTmpDir = testCase desc $ do
  tmpDir <- mkTestPath getTmpDir [osp|cdrtOverwritePartialFails|]
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> [osp|dest|]

  runEffPathWriter $ do
    createDirectoryIfMissing False destDir

    -- NOTE: test overwriting
    createDirectoryIfMissing False (destDir </> [osp|src|])
    createDirectoryIfMissing False (destDir </> [osp|src/test|])
    writeFiles [(destDir </> [osp|src/test/here|], "cat")]

  -- copy files
  result <-
    try $
      runPartialDynamicIO $
        PathWriter.copyDirectoryRecursiveConfig
          (overwriteConfig OverwriteDirectories)
          srcDir
          destDir
  resultEx <- case result of
    Right _ -> assertFailure "Expected exception, received none"
    Left (ex :: StringException) -> pure ex

  let exText = displayException resultEx

  assertBool exText ("Failed copying" `L.isInfixOf` exText)

  -- assert original files remain
  assertSrcExists tmpDir

  -- assert files were not copied over
  assertDirsDoNotExist $
    (destDir </>)
      <$> [ [osp|src/a/|],
            [osp|src/empty|]
          ]

  -- assert original file exists after copy failure
  assertFilesExist [destDir </> [osp|src/test/here|]]
  where
    desc = "Partial failure with extant dest/<target> rolls back changes"

cdrOverwriteAllTests :: IO OsPath -> TestTree
cdrOverwriteAllTests getTmpDir =
  testGroup
    "OverwriteAll"
    [ cdraOverwriteFileSucceeds getTmpDir
    ]

cdraOverwriteFileSucceeds :: IO OsPath -> TestTree
cdraOverwriteFileSucceeds getTmpDir = testCase desc $ do
  tmpDir <- mkTestPath getTmpDir [osp|cdraOverwriteFileSucceeds|]
  srcDir <- setupSrc tmpDir
  let destDir = tmpDir </> [osp|dest|]

  runEffPathWriter $ createDirectoryIfMissing True (destDir </> [osp|src/a/b/c|])

  -- NOTE: this line is what is tested
  runEffPathWriter $ writeFiles [(destDir </> [osp|src/a/b/c/f1|], "cat")]
  assertFileContents [(destDir </> [osp|src/a/b/c/f1|], "cat")]

  -- copy files
  runEffPathWriter $
    PathWriter.copyDirectoryRecursiveConfig
      (overwriteConfig OverwriteAll)
      srcDir
      destDir

  assertSrcExists tmpDir
  -- check contents actually overwritten
  assertFileContents [(destDir </> [osp|src/a/b/c/f1|], "1")]
  assertDestExists tmpDir
  where
    desc = "Copy to extant dest/<target>/file succeeds"

copyDirectoryRecursiveMiscTests :: IO OsPath -> TestTree
copyDirectoryRecursiveMiscTests getTmpDir =
  testGroup
    "Misc"
    [ copyTestData getTmpDir,
      copyDotDir getTmpDir,
      copyHidden getTmpDir,
      copyDirNoSrcException getTmpDir
    ]

copyTestData :: IO OsPath -> TestTree
copyTestData getTmpDir = testCase desc $ do
  tmpDir <- getTmpDir

  let dataDir = tmpDir </> [osp|data|]
      srcDir = dataDir
      destDir = tmpDir </> [osp|copyTestData|]

  runEffPathWriter $ createDirectoryIfMissing False destDir

  runEffPathWriter $
    PathWriter.copyDirectoryRecursiveConfig
      (overwriteConfig OverwriteNone)
      srcDir
      destDir

  assertFilesExist $
    (\p -> destDir </> [osp|data|] </> p)
      <$> [ [osp|.hidden|] </> [osp|f1|],
            [osp|bar|],
            [osp|baz|],
            [osp|foo|],
            [osp|dir1|] </> [osp|f|],
            [osp|dir2|] </> [osp|f|],
            [osp|dir3|] </> [osp|f|],
            [osp|dir3|] </> [osp|dir3.1|] </> [osp|f|]
          ]
  assertDirsExist $
    (\p -> destDir </> [osp|data|] </> p)
      <$> [ [osp|.hidden|],
            [osp|dir1|],
            [osp|dir2|],
            [osp|dir3|],
            [osp|dir3|] </> [osp|dir3.1|]
          ]
  -- Notice that while the link names are copied to the new location, of course
  -- the _targets_ still refer to the old location (dataDir).
  assertSymlinksExistTarget $
    (\(l, t) -> (destDir </> [osp|data|] </> l, dataDir </> t))
      <$> [ ([osp|l1|], [osp|foo|]),
            ([osp|l2|], [osp|dir2|]),
            ([osp|l3|], [osp|bad|])
          ]
  where
    desc = "Copies test data directory with hidden dirs, symlinks"

copyDotDir :: IO OsPath -> TestTree
copyDotDir getTmpDir = testCase desc $ do
  tmpDir <- mkTestPath getTmpDir [osp|copyDotDir|]

  let srcDir = tmpDir </> [osp|src-0.2.2|]
      destDir = tmpDir </> [osp|dest|]

  runEffPathWriter $ do
    createDirectoryIfMissing False tmpDir
    createDirectoryIfMissing False destDir
    createDirectoryIfMissing False srcDir
    writeFiles [(srcDir </> [osp|f|], "")]

  runEffPathWriter $
    PathWriter.copyDirectoryRecursiveConfig
      (overwriteConfig OverwriteNone)
      srcDir
      destDir

  assertDirsExist [destDir </> [osp|src-0.2.2|]]
  assertFilesExist [destDir </> [osp|src-0.2.2|] </> [osp|f|]]
  where
    desc = "Copies dir with dots in the name"

copyHidden :: IO OsPath -> TestTree
copyHidden getTmpDir = testCase desc $ do
  tmpDir <- mkTestPath getTmpDir [osp|copyHidden|]

  let srcDir = tmpDir </> [osp|.hidden|]
      destDir = tmpDir </> [osp|dest|]

  runEffPathWriter $ do
    createDirectoryIfMissing False tmpDir
    createDirectoryIfMissing False destDir
    createDirectoryIfMissing False srcDir
    writeFiles [(srcDir </> [osp|f|], "")]

  runEffPathWriter $
    PathWriter.copyDirectoryRecursiveConfig
      (overwriteConfig OverwriteDirectories)
      srcDir
      destDir

  assertDirsExist [destDir </> [osp|.hidden|]]
  assertFilesExist [destDir </> [osp|.hidden|] </> [osp|f|]]
  where
    desc = "Copies top-level hidden dir"

copyDirNoSrcException :: IO OsPath -> TestTree
copyDirNoSrcException getTmpDir = testCase desc $ do
  tmpDir <- mkTestPath getTmpDir [osp|copyDirNoSrcException|]

  let badSrc = tmpDir </> [osp|badSrc|]
      destDir = tmpDir </> [osp|dest|]

  runEffPathWriter $ do
    createDirectoryIfMissing False tmpDir
    createDirectoryIfMissing False destDir

  let copy =
        PathWriter.copyDirectoryRecursiveConfig
          (overwriteConfig OverwriteNone)
          badSrc
          destDir

  try @_ @IOException (runEffPathWriter copy) >>= \case
    Left _ -> pure ()
    Right _ -> assertFailure "Expected PathNotFoundException"
  where
    desc = "Bad source throws exception"

removeLinkTests :: IO OsPath -> TestTree
removeLinkTests getTestDir =
  testGroup
    "removeSymbolicLink"
    [ removeSymbolicLinkFileLink getTestDir,
      removeSymbolicLinkFileException getTestDir,
      removeSymbolicLinkBadException getTestDir
    ]

removeSymbolicLinkFileLink :: IO OsPath -> TestTree
removeSymbolicLinkFileLink getTestDir = testCase desc $ do
  testDir <- setupLinks getTestDir [osp|removeSymbolicLinkFileLink|]

  assertSymlinksExist $ (testDir </>) <$> [[osp|file-link|], [osp|dir-link|]]

  runEffPathWriter $ do
    PW.removeSymbolicLink (testDir </> [osp|file-link|])
    PW.removeSymbolicLink (testDir </> [osp|dir-link|])

  assertSymlinksDoNotExist $ (testDir </>) <$> [[osp|file-link|], [osp|dir-link|]]
  where
    desc = "Removes symbolic links"

removeSymbolicLinkFileException :: IO OsPath -> TestTree
removeSymbolicLinkFileException getTestDir = testCase desc $ do
  testDir <- setupLinks getTestDir [osp|removeSymbolicLinkFileLink|]
  let filePath = testDir </> [osp|file|]

  assertFilesExist [filePath]

  try @_ @IOException (runEffPathWriter $ PW.removeSymbolicLink filePath) >>= \case
    Left _ -> pure ()
    Right _ -> assertFailure "Expected IOException"

  assertFilesExist [filePath]
  where
    desc = "Exception for file"

removeSymbolicLinkBadException :: IO OsPath -> TestTree
removeSymbolicLinkBadException getTestDir = testCase desc $ do
  testDir <- setupLinks getTestDir [osp|removeSymbolicLinkBadException|]
  let filePath = testDir </> [osp|bad-path|]

  try @_ @IOException (runEffPathWriter $ PW.removeSymbolicLink filePath) >>= \case
    Left _ -> pure ()
    Right _ -> assertFailure "Expected IOException"
  where
    desc = "Exception for bad path"

copyLinkTests :: IO OsPath -> TestTree
copyLinkTests getTestDir =
  testGroup
    "copySymbolicLink"
    [ copySymbolicLinks getTestDir,
      copySymbolicLinkFileException getTestDir,
      copySymbolicLinkDirException getTestDir,
      copySymbolicLinkBadException getTestDir
    ]

copySymbolicLinks :: IO OsPath -> TestTree
copySymbolicLinks getTestDir = testCase desc $ do
  testDir <- setupLinks getTestDir [osp|copyFileLink|]
  let srcFile = testDir </> [osp|file-link|]
      srcDir = testDir </> [osp|dir-link|]
      destFile = testDir </> [osp|file-link2|]
      destDir = testDir </> [osp|dir-link2|]

  assertSymlinksExist [srcFile, srcDir]
  assertSymlinksDoNotExist [destFile, destDir]

  runEffPathWriter $ do
    PW.copySymbolicLink srcFile destFile
    PW.copySymbolicLink srcDir destDir

  assertSymlinksExistTarget
    [ (srcFile, testDir </> [osp|file|]),
      (destFile, testDir </> [osp|file|]),
      (srcDir, testDir </> [osp|dir|]),
      (destDir, testDir </> [osp|dir|])
    ]
  where
    desc = "Copies symbolic links"

copySymbolicLinkFileException :: IO OsPath -> TestTree
copySymbolicLinkFileException getTestDir = testCase desc $ do
  testDir <- setupLinks getTestDir [osp|copySymbolicLinkFileException|]
  let src = testDir </> [osp|file|]
      dest = testDir </> [osp|dest|]
  try @_ @IOException (runEffPathWriter $ PW.copySymbolicLink src dest) >>= \case
    Left _ -> pure ()
    Right _ -> assertFailure "Exception IOException"
  where
    desc = "Exception for file"

copySymbolicLinkDirException :: IO OsPath -> TestTree
copySymbolicLinkDirException getTestDir = testCase desc $ do
  testDir <- setupLinks getTestDir [osp|copySymbolicLinkDirException|]
  let src = testDir </> [osp|dir|]
      dest = testDir </> [osp|dest|]
  try @_ @IOException (runEffPathWriter $ PW.copySymbolicLink src dest) >>= \case
    Left _ -> pure ()
    Right _ -> assertFailure "Exception IOException"
  where
    desc = "Exception for directory"

copySymbolicLinkBadException :: IO OsPath -> TestTree
copySymbolicLinkBadException getTestDir = testCase desc $ do
  testDir <- setupLinks getTestDir [osp|copySymbolicLinkBadException|]
  let src = testDir </> [osp|bad-path|]
      dest = testDir </> [osp|dest|]
  try @_ @IOException (runEffPathWriter $ PW.copySymbolicLink src dest) >>= \case
    Left _ -> pure ()
    Right _ -> assertFailure "Exception IOException"
  where
    desc = "Exception for file"

-- NOTE: For removeExistsTests, we do not test all permutations. In particular,
-- we do not test symlinks as "bad paths" for e.g. removeFileIfExists or
-- removeDirIfExists because those functions are based on
-- does(file|directory)Exist, and those return True based on the _target_
-- for the link.
--
-- In other words, for an extant directory link, doesDirectoryExist will return
-- true, yet removeDirectory will throw an exception.
--
-- doesFileExist / removeFile will work on Posix because Posix treats symlinks
-- as files...but it wil fail on windows.
--
-- But we want to keep these functions as simple as possible i.e. the obvious
-- doesXExist -> removeX. So we don't maintain any illusion that these
-- functions are total for all possible path type inputs. Really you should
-- only use them when you know the type of your potential path.

removeExistsTests :: IO OsPath -> TestTree
removeExistsTests getTestDir =
  testGroup
    "removeXIfExists"
    [ removeFileIfExistsTrue getTestDir,
      removeFileIfExistsFalseBad getTestDir,
      removeFileIfExistsFalseWrongType getTestDir,
      removeDirIfExistsTrue getTestDir,
      removeDirIfExistsFalseBad getTestDir,
      removeDirIfExistsFalseWrongType getTestDir,
      removeSymlinkIfExistsTrue getTestDir,
      removeSymlinkIfExistsFalseBad getTestDir,
      removeSymlinkIfExistsFalseWrongType getTestDir
    ]

removeFileIfExistsTrue :: IO OsPath -> TestTree
removeFileIfExistsTrue getTestDir = testCase desc $ do
  testDir <- setupLinks getTestDir [osp|removeFileIfExistsTrue|]
  let file = testDir </> [osp|file|]
  assertFilesExist [file]

  runEffPathWriter $ PW.removeFileIfExists file

  assertFilesDoNotExist [file]
  where
    desc = "removeFileIfExists removes file"

removeFileIfExistsFalseBad :: IO OsPath -> TestTree
removeFileIfExistsFalseBad getTestDir = testCase desc $ do
  testDir <- setupLinks getTestDir [osp|removeFileIfExistsFalseBad|]
  let file = testDir </> [osp|bad-path|]
  assertFilesDoNotExist [file]

  runEffPathWriter $ PW.removeFileIfExists file

  assertFilesDoNotExist [file]
  where
    desc = "removeFileIfExists does nothing for bad path"

removeFileIfExistsFalseWrongType :: IO OsPath -> TestTree
removeFileIfExistsFalseWrongType getTestDir = testCase desc $ do
  testDir <- setupLinks getTestDir [osp|removeFileIfExistsFalseWrongType|]
  let dir = testDir </> [osp|dir|]

  assertDirsExist [dir]

  runEffPathWriter $ PW.removeFileIfExists dir

  assertDirsExist [dir]
  where
    desc = "removeFileIfExists does nothing for wrong file types"

removeDirIfExistsTrue :: IO OsPath -> TestTree
removeDirIfExistsTrue getTestDir = testCase desc $ do
  testDir <- setupLinks getTestDir [osp|removeDirIfExistsTrue|]
  let dir = testDir </> [osp|dir|]
  assertDirsExist [dir]

  runEffPathWriter $ PW.removeDirectoryIfExists dir

  assertDirsDoNotExist [dir]
  where
    desc = "removeDirectoryIfExists removes dir"

removeDirIfExistsFalseBad :: IO OsPath -> TestTree
removeDirIfExistsFalseBad getTestDir = testCase desc $ do
  testDir <- setupLinks getTestDir [osp|removeDirIfExistsFalseBad|]
  let dir = testDir </> [osp|bad-path|]
  assertDirsDoNotExist [dir]

  runEffPathWriter $ PW.removeDirectoryIfExists dir

  assertDirsDoNotExist [dir]
  where
    desc = "removeDirectoryIfExists does nothing for bad path"

removeDirIfExistsFalseWrongType :: IO OsPath -> TestTree
removeDirIfExistsFalseWrongType getTestDir = testCase desc $ do
  testDir <- setupLinks getTestDir [osp|removeDirIfExistsFalseWrongType|]
  let file = testDir </> [osp|file|]

  assertFilesExist [file]

  runEffPathWriter $ PW.removeDirectoryIfExists file

  assertFilesExist [file]
  where
    desc = "removeDirectoryIfExists does nothing for wrong file types"

removeSymlinkIfExistsTrue :: IO OsPath -> TestTree
removeSymlinkIfExistsTrue getTestDir = testCase desc $ do
  testDir <- setupLinks getTestDir [osp|removeSymlinkIfExistsTrue|]
  let fileLink = testDir </> [osp|file-link|]
      dirLink = testDir </> [osp|dir-link|]

  assertSymlinksExist [fileLink, dirLink]

  runEffPathWriter $ do
    PW.removeSymbolicLinkIfExists fileLink
    PW.removeSymbolicLinkIfExists dirLink

  assertSymlinksDoNotExist [fileLink, dirLink]
  where
    desc = "removeSymbolicLinkIfExists removes links"

removeSymlinkIfExistsFalseBad :: IO OsPath -> TestTree
removeSymlinkIfExistsFalseBad getTestDir = testCase desc $ do
  testDir <- setupLinks getTestDir [osp|removeSymlinkIfExistsFalseBad|]
  let link = testDir </> [osp|bad-path|]
  assertSymlinksDoNotExist [link]

  runEffPathWriter $ PW.removeSymbolicLinkIfExists link

  assertSymlinksDoNotExist [link]
  where
    desc = "removeSymbolicLinkIfExists does nothing for bad path"

removeSymlinkIfExistsFalseWrongType :: IO OsPath -> TestTree
removeSymlinkIfExistsFalseWrongType getTestDir = testCase desc $ do
  testDir <- setupLinks getTestDir [osp|removeSymlinkIfExistsFalseWrongType|]
  let file = testDir </> [osp|file|]
      dir = testDir </> [osp|dir|]

  assertFilesExist [file]
  assertDirsExist [dir]

  runEffPathWriter $ do
    PW.removeSymbolicLinkIfExists file
    PW.removeSymbolicLinkIfExists dir

  assertFilesExist [file]
  assertDirsExist [dir]
  where
    desc = "removeSymbolicLinkIfExists does nothing for wrong file types"

-------------------------------------------------------------------------------
--                                  Setup                                    --
-------------------------------------------------------------------------------

setupSrc :: OsPath -> IO OsPath
setupSrc = runEff . runFileWriterDynamicIO . runPathWriterDynamicIO . setupSrcEff

setupSrcEff :: (FileWriterDynamic :> es, IOE :> es, PathWriterDynamic :> es) => OsPath -> Eff es OsPath
setupSrcEff baseDir = do
  let files = [[osp|a/b/c/f1|], [osp|a/f2|], [osp|a/b/f3|], [osp|a/f4|], [osp|a/f5|], [osp|a/b/f5|]]
      srcDir = baseDir </> [osp|src|]

  -- create directories and files
  createDirectoryIfMissing True (srcDir </> [osp|a/b/c|])
  createDirectoryIfMissing True (srcDir </> [osp|empty/d|])

  let baseFiles = zip files ["1", "2", "3", "4", "5", "6"]
      srcFiles = fmap (first (srcDir </>)) baseFiles

  writeFiles srcFiles
  liftIO $ assertSrcExists baseDir
  pure srcDir

writeFiles :: (FileWriterDynamic :> es) => [(OsPath, ByteString)] -> Eff es ()
writeFiles = traverse_ (uncurry writeBinaryFile)

overwriteConfig :: Overwrite -> CopyDirConfig
overwriteConfig ow = MkCopyDirConfig ow TargetNameSrc

setupLinks :: IO OsPath -> OsPath -> IO OsPath
setupLinks getTestDir suffix = do
  testDir <- (\t -> t </> [osp|path-writer|] </> suffix) <$> getTestDir
  let fileLink = testDir </> [osp|file-link|]
      dirLink = testDir </> [osp|dir-link|]
      file = testDir </> [osp|file|]
      dir = testDir </> [osp|dir|]

  runEffPathWriter $ do
    PW.createDirectoryIfMissing True dir
    FW.writeBinaryFile file ""
    PW.createFileLink file fileLink
    PW.createDirectoryLink dir dirLink

  pure testDir

-------------------------------------------------------------------------------
--                                  Mock                                     --
-------------------------------------------------------------------------------

runPartialDynamicIO :: Eff [PathWriterDynamic, PathReaderDynamic, IOE] a -> IO a
runPartialDynamicIO effs = do
  counterRef <- newIORef 0

  runEff
    . runPathReaderDynamicIO
    . runMockWriter counterRef
    $ effs
  where
    runMockWriter ::
      ( IOE :> es
      ) =>
      IORef Int ->
      Eff (PathWriterDynamic : es) a ->
      Eff es a
    runMockWriter counterRef = reinterpret runPathWriterDynamicIO $ \_ -> \case
      CreateDirectory p -> createDirectory p
      CreateDirectoryIfMissing b p -> createDirectoryIfMissing b p
      RemoveDirectoryRecursive p -> removeDirectoryRecursive p
      RemoveDirectory p -> removeDirectory p
      RemoveFile p -> removeFile p
      CopyFileWithMetadata src dest -> do
        counter <- liftIO $ readIORef counterRef
        if counter > 3
          then throwString $ "Failed copying: " ++ show dest
          else liftIO $ modifyIORef' counterRef (+ 1)
        copyFileWithMetadata src dest
      _ -> throwString "unimplemented"

-------------------------------------------------------------------------------
--                                Assertions                                 --
-------------------------------------------------------------------------------

assertSrcExists :: OsPath -> IO ()
assertSrcExists baseDir = do
  let srcDir = baseDir </> [osp|src|]
  assertFilesExist $
    (srcDir </>)
      <$> [ [osp|a/b/c/f1|],
            [osp|a/f2|],
            [osp|a/b/f3|],
            [osp|a/f4|],
            [osp|a/f5|],
            [osp|a/b/f5|]
          ]
  assertDirsExist $
    (srcDir </>)
      <$> [ [osp|a/b/c|],
            [osp|empty/d|]
          ]

assertDestExists :: OsPath -> IO ()
assertDestExists baseDir = do
  let destDir = baseDir </> [osp|dest|]
  assertFilesExist $
    (destDir </>)
      <$> [ [osp|src/a/b/c/f1|],
            [osp|src/a/f2|],
            [osp|src/a/b/f3|],
            [osp|src/a/f4|],
            [osp|src/a/f5|],
            [osp|src/a/b/f5|]
          ]
  assertDirsExist $
    (destDir </>)
      <$> [ [osp|src/a/b/c|],
            [osp|src/empty/d|]
          ]

assertFilesExist :: [OsPath] -> IO ()
assertFilesExist = traverse_ $ \p -> do
  exists <- runEffPathWriter $ doesFileExist p
  assertBool ("Expected file to exist: " <> U.pathToStr p) exists

assertFilesDoNotExist :: [OsPath] -> IO ()
assertFilesDoNotExist = traverse_ $ \p -> do
  exists <- runEffPathWriter $ doesFileExist p
  assertBool ("Expected file not to exist: " <> U.pathToStr p) (not exists)

assertFileContents :: [(OsPath, ByteString)] -> IO ()
assertFileContents = traverse_ $ \(p, expected) -> do
  exists <- runEffPathWriter $ doesFileExist p
  assertBool ("Expected file to exist: " <> U.pathToStr p) exists
  actual <- runEff $ runFileReaderDynamicIO $ readBinaryFile p
  expected @=? actual

assertDirsExist :: [OsPath] -> IO ()
assertDirsExist = traverse_ $ \p -> do
  exists <- runEffPathWriter $ doesDirectoryExist p
  assertBool ("Expected directory to exist: " <> U.pathToStr p) exists

assertDirsDoNotExist :: [OsPath] -> IO ()
assertDirsDoNotExist = traverse_ $ \p -> do
  exists <- runEffPathWriter $ doesDirectoryExist p
  assertBool ("Expected directory not to exist: " <> U.pathToStr p) (not exists)

assertSymlinksExist :: [OsPath] -> IO ()
assertSymlinksExist = assertSymlinksExist' . fmap (,Nothing)

assertSymlinksExistTarget :: [(OsPath, OsPath)] -> IO ()
assertSymlinksExistTarget = assertSymlinksExist' . (fmap . fmap) Just

assertSymlinksExist' :: [(OsPath, Maybe OsPath)] -> IO ()
assertSymlinksExist' = traverse_ $ \(l, t) -> do
  exists <- runEffPathWriter $ PR.doesSymbolicLinkExist l
  assertBool ("Expected symlink to exist: " <> Utils.decodeOsToFpShow l) exists

  case t of
    Nothing -> pure ()
    Just expectedTarget -> do
      target <- runEffPathWriter $ PR.getSymbolicLinkTarget l
      expectedTarget @=? target

assertSymlinksDoNotExist :: [OsPath] -> IO ()
assertSymlinksDoNotExist = traverse_ $ \l -> do
  exists <- runEffPathWriter $ PR.doesSymbolicLinkExist l
  assertBool ("Expected symlink not to exist: " <> Utils.decodeOsToFpShow l) (not exists)

mkTestPath :: IO OsPath -> OsPath -> IO OsPath
mkTestPath getPath s = do
  p <- getPath
  pure $ p </> s

runEffPathWriter ::
  Eff
    '[ PathReaderDynamic,
       PathWriterDynamic,
       FileWriterDynamic,
       IOE
     ]
    a ->
  IO a
runEffPathWriter =
  runEff
    . runFileWriterDynamicIO
    . runPathWriterDynamicIO
    . runPathReaderDynamicIO
