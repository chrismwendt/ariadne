{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module InvocationTests (invocationTests) where

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

invocationTests =
  withResource connect closeConnection $ \getTransport ->
  withResource createRun (const cleanRun) $ \_ ->
    testGroup "Tests"
      [ mkTest "Local var, same file" getTransport
        ("t1.hs", 5, 6)
        (TupleTerm [AtomTerm "loc_known",BinaryTerm "t1.hs",IntTerm 4,IntTerm 7])
        (return ())
      , mkTest "Global var, different file" getTransport
        ("t1.hs", 7, 9)
        (TupleTerm [AtomTerm "loc_known",BinaryTerm "L.hs",IntTerm 3,IntTerm 1])
        (return ())
      , mkTest "Global var, not defined yet" getTransport
        ("t1.hs", 9, 9)
        (TupleTerm [AtomTerm "error"])
        (return ())
      , mkTest "Global var, just defined" getTransport
        ("t1.hs", 9, 9)
        (TupleTerm [AtomTerm "loc_known",BinaryTerm "L.hs",IntTerm 3,IntTerm 1])
        (copyFile (runDir </> "Lv2.hs") (runDir </> "L.hs"))
      , mkTest "Recursive modules" getTransport
        ("Rec1.hs", 5, 5)
        (TupleTerm [AtomTerm "loc_known",BinaryTerm "Rec2.hs",IntTerm 5,IntTerm 1])
        (return ())
      ]
  where
    connect =
      tcpClient "localhost" 39014 `catch` \(e :: IOException) ->
        throwIO $ ErrorCall "Failed to connect; is ariadne-server running?"

testSourcesDir = "tests/test-sources"
runDir = "tests/run"

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

mkTest :: Transport t => String -> IO t -> Loc -> Term -> IO () -> TestTree
mkTest name getTransport (f1, l1, c1) response prepare = testCase name $ do
  t <- getTransport
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
