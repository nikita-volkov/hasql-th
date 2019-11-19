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
    checkParallel $ Group "Parsing a rendered AST produces the same AST" $ let
      p _name _gen _parser _renderer =
        (,) _name $ withTests 10000 $ property $ do
          ast <- forAll _gen
          let
            sql = Rendering.toText (_renderer ast)
            in do
              footnote ("SQL: " <> Text.unpack sql)
              case Parsing.parse _parser sql of
                Left err -> do
                  footnote (Text.unpack err)
                  failure
                Right ast' -> ast === ast'
      in [
          p "literal" Gen.literal Parsing.literal Rendering.literal
          ,
          p "aExpr" Gen.expr Parsing.aExpr Rendering.expr
          ,
          p "preparableStmt" Gen.preparableStmt Parsing.preparableStmt Rendering.preparableStmt
        ]
  ]
