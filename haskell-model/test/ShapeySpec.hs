module ShapeySpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Control.Lens hiding (elements)
import qualified Data.Map.Strict as M
import Linear

import Abyme.Direction
import Abyme.Polyomino
import Abyme.Shapey.Universe
import Abyme.Shapey.Chunk
import Abyme.Shapey.Universe.Generate
import Abyme.Shapey.Universe.Validate

randomChunk :: Universe -> Gen Chunk
randomChunk u = shapeChunk u <$> randomShape u

spec :: Spec
spec = do
  describe "addressing" $ do
    prop "square occupies its location" $ \u -> do
      s <- randomSquare u
      return $ Just s === inhabitant u (squareLocation u s)

    prop "location can be nudged and back" $ \u -> do
      s <- randomSquare u
      d <- arbitrary
      let l = squareLocation u s
      return $ case nudgeLocation u d l of
        Nothing -> True
        Just l' -> nudgeLocation u (directionOpposite d) l' == Just l

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

  describe "chunk" $ do
    prop "shape is member of its chunk" $ \u -> do
      s <- randomShape u

      return $ s `elem` shapeChunk u s ^. chunkTopShapes

  -- It's possible these will only work if we're moving the loop
  describe "movement" $ do
    prop "result of a move is valid" $ \u -> do
      d <- arbitrary
      c <- randomChunk u

      return $ canPushChunk u d c ==> validateUniverse (pushChunk u d c)

    prop "after moving can move back" $ \u -> do
      d <- arbitrary
      c <- randomChunk u

      let (u', c') = pushChunkWithResult u d c
      return $ canPushChunk u d c ==> canPushChunk u' (directionOpposite d) c'
