module Main where

import Hasql.TH.Prelude
import Hedgehog
import Hedgehog.Main
import qualified Main.Gen as Gen
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Hasql.TH.Syntax.Ast as Ast
import qualified Hasql.TH.Syntax.Parsing as Parsing
import qualified Hasql.TH.Syntax.Rendering as Rendering
import qualified Data.Text as Text


main = defaultMain [
    checkParallel $ Group "Bug-fixes" $ let
      parsesFine testName sql =
        (,) (testName <> " parses fine") $ property $ test $ do
          footnote (Text.unpack sql)
          case Parsing.run Parsing.preparableStmt sql of
            Left err -> do
              footnote err
              failure
            Right _ -> success
      in [
          parsesFine "Multiple inner joins"
            "select i :: int8 from auth.user as u\n\
            \inner join edgenode.usere_provider as p\n\
            \on u.id = p.user_id\n\
            \inner join edgenode.provider_branch as b\n\
            \on b.provider_fk = p.provider_id"
        ]
    ,
    checkParallel $ Group "Parsing a rendered AST produces the same AST" $ let
      p _name _amount _gen _parser _renderer =
        (,) _name $ withDiscards (fromIntegral _amount * 200) $ withTests _amount $ property $ do
          ast <- forAll _gen
          let
            sql = Rendering.toText (_renderer ast)
            in do
              footnote ("SQL: " <> Text.unpack sql)
              case Parsing.run _parser sql of
                Left err -> do
                  footnote err
                  failure
                Right ast' -> ast === ast'
      in [
          p "tableRef" 10000 Gen.tableRef Parsing.tableRef Rendering.tableRef
          ,
          p "aExpr" 100000 Gen.aExpr Parsing.aExpr Rendering.aExpr
          ,
          p "preparableStmt" 35000 Gen.preparableStmt Parsing.preparableStmt Rendering.preparableStmt
        ]
  ]
