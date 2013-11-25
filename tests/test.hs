{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

import Data.BERT
import Network.BERT.Client
import Network.BERT.Transport
import System.Directory
import System.FilePath
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Control.Exception
import Control.Monad
import Test.Tasty
import Test.Tasty.HUnit

main = do
  t <- tcpClient "localhost" 39014 `catch` \(e :: IOException) ->
    throwIO $ ErrorCall "Failed to connect; is ariadne-server running?"
  bracket_ createRun cleanRun $
    defaultMain $ testGroup "Tests"
      [ mkTest "Local var, same file" t
        ("t1.hs", 5, 6)
        (TupleTerm [AtomTerm "loc_known",BinaryTerm "t1.hs",IntTerm 4,IntTerm 7])
        (return ())
      , mkTest "Global var, different file" t
        ("t1.hs", 7, 9)
        (TupleTerm [AtomTerm "loc_known",BinaryTerm "L.hs",IntTerm 3,IntTerm 1])
        (return ())
      , mkTest "Global var, not defined yet" t
        ("t1.hs", 9, 9)
        (TupleTerm [AtomTerm "error"])
        (return ())
      , mkTest "Global var, just defined" t
        ("t1.hs", 9, 9)
        (TupleTerm [AtomTerm "loc_known",BinaryTerm "L.hs",IntTerm 3,IntTerm 1])
        (copyFile (runDir </> "Lv2.hs") (runDir </> "L.hs"))
      ]

testSourcesDir = "test-sources"
runDir = "run"

createRun = do
  createDirectory runDir
  files <- getDirectoryContents testSourcesDir
  forM_ files $ \f -> do
    let fp = testSourcesDir </> f
    exists <- doesFileExist fp
    when exists $
      copyFile fp (runDir </> f)

cleanRun = removeDirectoryRecursive runDir

type Loc = (FilePath, Int {-line-}, Int {-col-})

mkTest :: Transport t => String -> t -> Loc -> Term -> IO () -> TestTree
mkTest name t (f1, l1, c1) response prepare = testCase name $ do
  prepare
  f <- canonicalizePath $ runDir </> f1
  r <- call t "ariadne" "find" [BinaryTerm (UTF8.fromString f), IntTerm l1, IntTerm c1]
  case r of
    Left err -> assertFailure $ show err
    Right r -> relativize r @?= response

relativize (TupleTerm [a@(AtomTerm "loc_known"), BinaryTerm path, l, c]) =
  TupleTerm [a, BinaryTerm (UTF8.fromString . takeFileName . UTF8.toString $ path), l, c]
relativize (TupleTerm [a@(AtomTerm "error"),BinaryTerm _]) = TupleTerm [a]
relativize x = x
