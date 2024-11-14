{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

module Unit.PathReader (tests) where

import Control.Exception.Utils (trySync)
import Data.List qualified as L
import Effectful (Eff, IOE)
import Effectful.FileSystem.FileReader.Static qualified as FR
import Effectful.FileSystem.FileWriter.Static qualified as FW
import Effectful.FileSystem.PathReader.Static
  ( PathType
      ( PathTypeDirectory,
        PathTypeFile,
        PathTypeSymbolicLink
      ),
  )
import Effectful.FileSystem.PathReader.Static qualified as PR
import Effectful.FileSystem.PathWriter.Static qualified as PW
import FileSystem.OsPath (OsPath, osp, (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@=?))
import Unit.TestUtils qualified as TestUtils

tests :: IO OsPath -> TestTree
tests getTmpDir =
  testGroup
    "PathReader"
    [ listDirectoryTests getTmpDir,
      symlinkTests getTmpDir,
      pathTypeTests getTmpDir
    ]

listDirectoryTests :: IO OsPath -> TestTree
listDirectoryTests getTmpDir =
  testGroup
    "listDirectoryRecursive"
    [ testListDirectoryRecursive,
      testListDirectoryRecursiveSymlinkTargets getTmpDir,
      testListDirectoryRecursiveSymbolicLink getTmpDir
    ]

testListDirectoryRecursive :: TestTree
testListDirectoryRecursive = testCase "Recursively lists sub-files/dirs" $ do
  (files, dirs) <-
    TestUtils.runTestEff $ PR.listDirectoryRecursive [osp|src|]
  let (files', dirs') = (L.sort files, L.sort dirs)
  expectedFiles @=? files'
  expectedDirs @=? dirs'
  where
    expectedFiles =
      [ [osp|Effectful|] </> [osp|FileSystem|] </> [osp|FileReader|] </> [osp|Dynamic.hs|],
        [osp|Effectful|] </> [osp|FileSystem|] </> [osp|FileReader|] </> [osp|Static.hs|],
        [osp|Effectful|] </> [osp|FileSystem|] </> [osp|FileWriter|] </> [osp|Dynamic.hs|],
        [osp|Effectful|] </> [osp|FileSystem|] </> [osp|FileWriter|] </> [osp|Static.hs|],
        [osp|Effectful|] </> [osp|FileSystem|] </> [osp|HandleReader|] </> [osp|Dynamic.hs|],
        [osp|Effectful|] </> [osp|FileSystem|] </> [osp|HandleReader|] </> [osp|Static.hs|],
        [osp|Effectful|] </> [osp|FileSystem|] </> [osp|HandleWriter|] </> [osp|Dynamic.hs|],
        [osp|Effectful|] </> [osp|FileSystem|] </> [osp|HandleWriter|] </> [osp|Static.hs|],
        [osp|Effectful|] </> [osp|FileSystem|] </> [osp|PathReader|] </> [osp|Dynamic.hs|],
        [osp|Effectful|] </> [osp|FileSystem|] </> [osp|PathReader|] </> [osp|Static.hs|],
        [osp|Effectful|] </> [osp|FileSystem|] </> [osp|PathWriter|] </> [osp|Dynamic.hs|],
        [osp|Effectful|] </> [osp|FileSystem|] </> [osp|PathWriter|] </> [osp|Static.hs|],
        [osp|Effectful|] </> [osp|FileSystem|] </> [osp|PathWriter|] </> [osp|Utils.hs|]
      ]
    expectedDirs =
      [ [osp|Effectful|],
        [osp|Effectful|] </> [osp|FileSystem|],
        [osp|Effectful|] </> [osp|FileSystem|] </> [osp|FileReader|],
        [osp|Effectful|] </> [osp|FileSystem|] </> [osp|FileWriter|],
        [osp|Effectful|] </> [osp|FileSystem|] </> [osp|HandleReader|],
        [osp|Effectful|] </> [osp|FileSystem|] </> [osp|HandleWriter|],
        [osp|Effectful|] </> [osp|FileSystem|] </> [osp|PathReader|],
        [osp|Effectful|] </> [osp|FileSystem|] </> [osp|PathWriter|]
      ]

testListDirectoryRecursiveSymlinkTargets :: IO OsPath -> TestTree
testListDirectoryRecursiveSymlinkTargets getTmpDir = testCase desc $ do
  tmpDir <- getTmpDir
  let dataDir = tmpDir </> [osp|data|]

  (files, dirs) <-
    TestUtils.runTestEff $ PR.listDirectoryRecursive dataDir
  let (files', dirs') = (L.sort files, L.sort dirs)

  expectedFiles @=? files'
  expectedDirs @=? dirs'
  where
    desc = "Symlinks are categorized via targets"
    expectedFiles =
      [ [osp|.hidden|] </> [osp|f1|],
        [osp|bar|],
        [osp|baz|],
        [osp|dir1|] </> [osp|f|],
        [osp|dir2|] </> [osp|f|],
        [osp|dir3|] </> [osp|dir3.1|] </> [osp|f|],
        [osp|dir3|] </> [osp|f|],
        [osp|foo|],
        [osp|l1|],
        [osp|l2|] </> [osp|f|],
        [osp|l3|]
      ]
    expectedDirs =
      [ [osp|.hidden|],
        [osp|dir1|],
        [osp|dir2|],
        [osp|dir3|],
        [osp|dir3|] </> [osp|dir3.1|],
        [osp|l2|]
      ]

testListDirectoryRecursiveSymbolicLink :: IO OsPath -> TestTree
testListDirectoryRecursiveSymbolicLink getTmpDir = testCase desc $ do
  tmpDir <- getTmpDir
  let dataDir = tmpDir </> [osp|data|]

  (files, dirs, symlinks) <-
    TestUtils.runTestEff $ PR.listDirectoryRecursiveSymbolicLink dataDir
  let (files', dirs', symlinks') = (L.sort files, L.sort dirs, L.sort symlinks)

  expectedFiles @=? files'
  expectedDirs @=? dirs'
  expectedSymlinks @=? symlinks'
  where
    desc = "Recursively lists sub-files/dirs/symlinks"
    expectedFiles =
      [ [osp|.hidden|] </> [osp|f1|],
        [osp|bar|],
        [osp|baz|],
        [osp|dir1|] </> [osp|f|],
        [osp|dir2|] </> [osp|f|],
        [osp|dir3|] </> [osp|dir3.1|] </> [osp|f|],
        [osp|dir3|] </> [osp|f|],
        [osp|foo|]
      ]
    expectedDirs =
      [ [osp|.hidden|],
        [osp|dir1|],
        [osp|dir2|],
        [osp|dir3|],
        [osp|dir3|] </> [osp|dir3.1|]
      ]
    expectedSymlinks =
      [ [osp|l1|],
        [osp|l2|],
        [osp|l3|]
      ]

symlinkTests :: IO OsPath -> TestTree
symlinkTests getTestDir =
  testGroup
    "Symlinks"
    ( [ doesSymbolicLinkExistTrue getTestDir,
        doesSymbolicLinkExistFileFalse getTestDir,
        doesSymbolicLinkExistDirFalse getTestDir,
        doesSymbolicLinkExistBadFalse getTestDir,
        pathIsSymbolicDirectoryLinkTrue getTestDir,
        pathIsSymbolicFileLinkTrue getTestDir,
        pathIsSymbolicFileLinkFileFalse getTestDir,
        pathIsSymbolicFileLinkBad getTestDir
      ]
        ++ windowsTests getTestDir
    )

doesSymbolicLinkExistTrue :: IO OsPath -> TestTree
doesSymbolicLinkExistTrue getTestDir = testCase desc $ do
  testDir <- TestUtils.setupLinks getTestDir [osp|doesSymbolicLinkExistTrue|]

  fileLinkExists <-
    TestUtils.runTestEff $ PR.doesSymbolicLinkExist (testDir </> [osp|file-link|])
  assertBool "doesSymbolicLinkExist true for file link" fileLinkExists

  dirLinkExists <-
    TestUtils.runTestEff $ PR.doesSymbolicLinkExist (testDir </> [osp|dir-link|])
  assertBool "doesSymbolicLinkExist true for dir link" dirLinkExists
  where
    desc = "doesSymbolicLinkExist true for symlinks"

doesSymbolicLinkExistFileFalse :: IO OsPath -> TestTree
doesSymbolicLinkExistFileFalse getTestDir = testCase desc $ do
  testDir <- TestUtils.setupLinks getTestDir [osp|doesSymbolicLinkExistFileFalse|]

  linkExists <-
    TestUtils.runTestEff $ PR.doesSymbolicLinkExist (testDir </> [osp|file|])
  assertBool "doesSymbolicLinkExist false for file" (not linkExists)
  where
    desc = "doesSymbolicLinkExist false for file"

doesSymbolicLinkExistDirFalse :: IO OsPath -> TestTree
doesSymbolicLinkExistDirFalse getTestDir = testCase desc $ do
  testDir <- TestUtils.setupLinks getTestDir [osp|doesSymbolicLinkExistDirFalse|]

  linkExists <-
    TestUtils.runTestEff $ PR.doesSymbolicLinkExist (testDir </> [osp|dir|])
  assertBool "doesSymbolicLinkExist false for dir" (not linkExists)
  where
    desc = "doesSymbolicLinkExist false for dir"

doesSymbolicLinkExistBadFalse :: IO OsPath -> TestTree
doesSymbolicLinkExistBadFalse getTestDir = testCase desc $ do
  testDir <- TestUtils.setupLinks getTestDir [osp|doesSymbolicLinkExistBadFalse|]

  linkExists <-
    TestUtils.runTestEff $ PR.doesSymbolicLinkExist (testDir </> [osp|bad-path|])
  assertBool "doesSymbolicLinkExist false for bad path" (not linkExists)
  where
    desc = "doesSymbolicLinkExist false for bad path"

{- ORMOLU_DISABLE -}

pathIsSymbolicDirectoryLinkTrue :: IO OsPath -> TestTree
pathIsSymbolicDirectoryLinkTrue getTestDir = testCase desc $ do
  testDir <- TestUtils.setupLinks getTestDir [osp|pathIsSymbolicDirectoryLinkTrue|]

  isDirLink <-
    TestUtils.runTestEff $ PR.pathIsSymbolicDirectoryLink (testDir </> [osp|dir-link|])
  assertBool "pathIsSymbolicDirectoryLink true for dir link" isDirLink
  where
    desc = "pathIsSymbolicDirectoryLink true for dir link"

pathIsSymbolicFileLinkTrue :: IO OsPath -> TestTree
pathIsSymbolicFileLinkTrue getTestDir = testCase desc $ do
  testDir <- TestUtils.setupLinks getTestDir [osp|pathIsSymbolicFileLinkTrue|]

  isFileLink <-
    TestUtils.runTestEff $ PR.pathIsSymbolicFileLink (testDir </> [osp|file-link|])
  assertBool "pathIsSymbolicFileLink true for file link" isFileLink
  where
    desc = "pathIsSymbolicDirectoryLink true for file link"

windowsTests :: IO OsPath -> [TestTree]
#if WINDOWS
windowsTests getTestDir =
  [ pathIsSymbolicDirectoryLinkFalse getTestDir,
    pathIsSymbolicFileLinkFalse getTestDir
  ]

pathIsSymbolicDirectoryLinkFalse :: IO OsPath -> TestTree
pathIsSymbolicDirectoryLinkFalse getTestDir = testCase desc $ do
  testDir <- TestUtils.setupLinks getTestDir [osp|pathIsSymbolicDirectoryLinkFalse|]

  isDirLink <- TestUtils.runTestEff $ PR.pathIsSymbolicDirectoryLink (testDir </> [osp|file-link|])
  assertBool "pathIsSymbolicDirectoryLink false for windows file link" (not isDirLink)
  where
    desc = "pathIsSymbolicDirectoryLink false for windows file link"

pathIsSymbolicFileLinkFalse :: IO OsPath -> TestTree
pathIsSymbolicFileLinkFalse getTestDir = testCase desc $ do
  testDir <- TestUtils.setupLinks getTestDir [osp|pathIsSymbolicFileLinkFalse|]

  isFileLink <- TestUtils.runTestEff $ PR.pathIsSymbolicFileLink (testDir </> [osp|dir-link|])
  assertBool "pathIsSymbolicFileLink false for windows dir link" (not isFileLink)
  where
    desc = "pathIsSymbolicFileLink false for windows dir link"
#else
windowsTests _ = [ ]
#endif

{- ORMOLU_ENABLE -}

pathIsSymbolicFileLinkFileFalse :: IO OsPath -> TestTree
pathIsSymbolicFileLinkFileFalse getTestDir = testCase desc $ do
  testDir <- TestUtils.setupLinks getTestDir [osp|pathIsSymbolicFileLinkFileFalse|]

  throwIfNoEx $ PR.pathIsSymbolicFileLink (testDir </> [osp|file|])

  throwIfNoEx $ PR.pathIsSymbolicFileLink (testDir </> [osp|dir|])

  throwIfNoEx $ PR.pathIsSymbolicDirectoryLink (testDir </> [osp|file|])

  throwIfNoEx $ PR.pathIsSymbolicDirectoryLink (testDir </> [osp|dir|])
  where
    desc = "pathIsSymbolicXLink exception for non symlinks"

pathIsSymbolicFileLinkBad :: IO OsPath -> TestTree
pathIsSymbolicFileLinkBad getTestDir = testCase desc $ do
  testDir <- TestUtils.setupLinks getTestDir [osp|pathIsSymbolicFileLinkBad|]

  throwIfNoEx $ PR.pathIsSymbolicFileLink (testDir </> [osp|bad|])

  throwIfNoEx $ PR.pathIsSymbolicDirectoryLink (testDir </> [osp|bad|])
  where
    desc = "pathIsSymbolicXLink exception for bad path"

pathTypeTests :: IO OsPath -> TestTree
pathTypeTests getTestDir =
  testGroup
    "PathType"
    [ getPathTypeSymlink getTestDir,
      getPathTypeDirectory getTestDir,
      getPathTypeFile getTestDir,
      getPathTypeBad getTestDir
    ]

getPathTypeSymlink :: IO OsPath -> TestTree
getPathTypeSymlink getTestDir = testCase desc $ do
  testDir <- TestUtils.setupLinks getTestDir [osp|getPathTypeSymlink|]

  let link1 = testDir </> [osp|file-link|]
      link2 = testDir </> [osp|dir-link|]

  -- getPathType
  pathType1 <- TestUtils.runTestEff $ PR.getPathType link1
  PathTypeSymbolicLink @=? pathType1

  -- isPathType
  isSymlink <- TestUtils.runTestEff $ PR.isPathType PathTypeSymbolicLink link1
  assertBool "Should be a symlink" isSymlink

  isDirectory <- TestUtils.runTestEff $ PR.isPathType PathTypeDirectory link1
  assertBool "Should not be a directory" (not isDirectory)

  isFile <- TestUtils.runTestEff $ PR.isPathType PathTypeFile link1
  assertBool "Should not be a file" (not isFile)

  -- throwIfWrongPathType
  TestUtils.runTestEff $ throwHelper PathTypeSymbolicLink link1
  throwIfNoEx $ throwHelper PathTypeDirectory link1
  throwIfNoEx $ throwHelper PathTypeFile link1

  -- getPathType
  pathType2 <- TestUtils.runTestEff $ PR.getPathType (testDir </> [osp|dir-link|])
  PathTypeSymbolicLink @=? pathType2

  -- isPathType
  isSymlink2 <- TestUtils.runTestEff $ PR.isPathType PathTypeSymbolicLink link2
  assertBool "Should be a symlink" isSymlink2

  isDirectory2 <- TestUtils.runTestEff $ PR.isPathType PathTypeDirectory link2
  assertBool "Should not be a directory" (not isDirectory2)

  isFile2 <- TestUtils.runTestEff $ PR.isPathType PathTypeFile link2
  assertBool "Should not be a file" (not isFile2)

  -- throwIfWrongPathType
  TestUtils.runTestEff $ throwHelper PathTypeSymbolicLink link2
  throwIfNoEx $ throwHelper PathTypeDirectory link2
  throwIfNoEx $ throwHelper PathTypeFile link2
  where
    desc = "getPathType recognizes symlinks"
    throwHelper = PR.throwIfWrongPathType "getPathTypeSymlink"

getPathTypeDirectory :: IO OsPath -> TestTree
getPathTypeDirectory getTestDir = testCase desc $ do
  testDir <- TestUtils.setupLinks getTestDir [osp|getPathTypeDirectory|]

  -- getPathType
  pathType <- TestUtils.runTestEff $ PR.getPathType testDir
  PathTypeDirectory @=? pathType

  -- isPathType
  isSymlink <- TestUtils.runTestEff $ PR.isPathType PathTypeSymbolicLink testDir
  assertBool "Should not be a symlink" (not isSymlink)

  isDirectory <- TestUtils.runTestEff $ PR.isPathType PathTypeDirectory testDir
  assertBool "Should be a directory" isDirectory

  isFile <- TestUtils.runTestEff $ PR.isPathType PathTypeFile testDir
  assertBool "Should not be a file" (not isFile)

  -- throwIfWrongPathType
  throwIfNoEx $ throwHelper PathTypeSymbolicLink testDir
  TestUtils.runTestEff $ throwHelper PathTypeDirectory testDir
  throwIfNoEx $ throwHelper PathTypeFile testDir
  where
    desc = "getPathType recognizes directories"
    throwHelper = PR.throwIfWrongPathType "getPathTypeDirectory"

getPathTypeFile :: IO OsPath -> TestTree
getPathTypeFile getTestDir = testCase desc $ do
  testDir <- TestUtils.setupLinks getTestDir [osp|getPathTypeFile|]
  let path = testDir </> [osp|file|]

  -- getPathType
  pathType <- TestUtils.runTestEff $ PR.getPathType path
  PathTypeFile @=? pathType

  -- isPathType
  isSymlink <- TestUtils.runTestEff $ PR.isPathType PathTypeSymbolicLink path
  assertBool "Should not be a symlink" (not isSymlink)

  isDirectory <- TestUtils.runTestEff $ PR.isPathType PathTypeDirectory path
  assertBool "Should not be a directory" (not isDirectory)

  isFile <- TestUtils.runTestEff $ PR.isPathType PathTypeFile path
  assertBool "Should be a file" isFile

  -- throwIfWrongPathType
  throwIfNoEx $ throwHelper PathTypeSymbolicLink path
  throwIfNoEx $ throwHelper PathTypeDirectory path
  TestUtils.runTestEff $ PR.throwIfWrongPathType "" PathTypeFile path
  where
    desc = "getPathType recognizes files"
    throwHelper = PR.throwIfWrongPathType "getPathTypeFile"

getPathTypeBad :: IO OsPath -> TestTree
getPathTypeBad getTestDir = testCase desc $ do
  testDir <- TestUtils.setupLinks getTestDir [osp|getPathTypeBad|]

  eResult <- TestUtils.runTestEff $ trySync $ PR.getPathType (testDir </> [osp|bad file|])

  case eResult of
    Left _ -> pure ()
    Right _ -> assertFailure "Expected exception, received none"
  where
    desc = "getPathType throws exception for non-extant path"

throwIfNoEx ::
  Eff
    '[ PR.PathReader,
       PW.PathWriter,
       FR.FileReader,
       FW.FileWriter,
       IOE
     ]
    a ->
  IO ()
throwIfNoEx m = do
  eResult <- TestUtils.runTestEff $ trySync m
  case eResult of
    Left _ -> pure ()
    Right _ -> assertFailure "Expected exception, received none"
