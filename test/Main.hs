{-# LANGUAgE QuasiQuotes #-}

import MPrelude
import MIO.Core (runMIO)

import qualified CBT
import qualified CBT.Container
import qualified DBT.Postgresql            as DBT
import qualified DBT.Postgresql.Connection as DBT
import qualified DBT.Postgresql.Container  as DBT
import qualified Devtools
import qualified Hasql.Session             as Hasql
import qualified MHasql.TH                 as MHasql
import qualified Test.Tasty                as Tasty
import qualified Test.Tasty.HUnit          as Tasty

main :: IO ()
main = do
  CBT.runDefaultEnvironment $ do
    containerName <- CBT.Container.nextName (CBT.Container.Prefix "mhasql-test")
    DBT.withDatabaseContainerDefault containerName $ \clientConfig ->
      liftIO . Tasty.defaultMain $ Tasty.testGroup "mhasql"
        [ Devtools.testTree $$(Devtools.readDependencies [Devtools.Target "mhasql-th"])
        , testDB clientConfig
        ]

testDB :: DBT.ClientConfig -> Tasty.TestTree
testDB clientConfig =
  Tasty.testCase "smoke test" $
    Tasty.assertEqual "" (pure True) =<< runMIO ()
      (DBT.withConnectionSession clientConfig $ Hasql.statement () [MHasql.singletonStatement|SELECT true :: bool|])
