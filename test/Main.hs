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
        (,) _name $ property $ do
          ast <- forAll _gen
          let
            sql = Rendering.toText (_renderer ast)
            parsing = Parsing.parse _parser sql
            in do
              footnote ("SQL: " <> Text.unpack sql)
              Right ast === parsing
      in [
          p "literal" Gen.literal Parsing.literal Rendering.literal
          ,
          p "aExpr" Gen.expr Parsing.aExpr Rendering.expr
          ,
          p "preparableStmt" Gen.preparableStmt Parsing.preparableStmt Rendering.preparableStmt
        ]
  ]
