module ShapeySpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Control.Lens hiding (elements)
import qualified Data.Map.Strict as M
import Linear

import Abyme.Polyomino
import Abyme.Shapey.Universe
import Abyme.Shapey.Universe.Generate
import Abyme.Shapey.Universe.Validate

spec :: Spec
spec = do
  describe "addressing" $ do
    prop "square occupies its location" $ \u -> do
      s <- randomSquare u
      return $ Just s === inhabitant u (squareLocation u s)

    describe "tricky examples" $ do
      it "works for example 1" $
        let u = Universe {_universeShapes = M.fromList [
                           (ShapeId {getShapeId = 1},Shape {_shapeId = ShapeId {getShapeId = 1},
                                                            _shapeParentIds = [(ShapeId {getShapeId = 2},V2 0 0)],
                                                            _shapePolyomino = Polyomino {_polyominoSquares = [V2 0 0]}}),
                           (ShapeId {getShapeId = 2},Shape {_shapeId = ShapeId {getShapeId = 2},
                                                            _shapeParentIds = [(ShapeId {getShapeId = 1},V2 0 0)],
                                                            _shapePolyomino = Polyomino {_polyominoSquares = [V2 0 0]}}),
                           (ShapeId {getShapeId = 3},Shape {_shapeId = ShapeId {getShapeId = 3},
                                                            _shapeParentIds = [(ShapeId {getShapeId = 2},V2 1 0)],
                                                            _shapePolyomino = Polyomino {_polyominoSquares = [V2 0 0]}}),
                           (ShapeId {getShapeId = 4},Shape {_shapeId = ShapeId {getShapeId = 4},
                                                            _shapeParentIds = [(ShapeId {getShapeId = 1},V2 1 1), (ShapeId {getShapeId = 3},V2 (-1) 1)],
                                                            _shapePolyomino = Polyomino {_polyominoSquares = [V2 0 0, V2 1 0]}})]}

            s = Square {_squareShape = Shape {_shapeId = ShapeId {getShapeId = 4},
                                            _shapeParentIds = [(ShapeId {getShapeId = 1},V2 1 1), (ShapeId {getShapeId = 3},V2 (-1) 1)],
                                            _shapePolyomino = Polyomino {_polyominoSquares = [V2 0 0, V2 1 0]}},
                      _squarePosition = V2 0 0}

        in Just s === inhabitant u (squareLocation u s)

  describe "generator" $ do
    prop "generates valid universes" $ \u ->
      validateUniverse u

    prop "shrinks universes to valid universes" $ \u ->
      let shrinks = shrink u in
      not (null shrinks) ==> forAll (elements shrinks) $ validateUniverse
