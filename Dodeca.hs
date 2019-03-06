module Main where

import Data.List as L
import Data.Map as M
import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>))
import Control.Monad
import qualified Data.Text.Lazy as T
import qualified Data.Text.IO as T

import qualified Data.GraphViz as G
import qualified Data.GraphViz.Attributes.Complete as G
import qualified Data.GraphViz.Types as G

type Vertex = Int
data Color = R | G | B deriving (Show, Eq, Ord)
type Coloring = Map Vertex Color

vertices :: [Vertex]
vertices = [1..30]

triangles :: [(Vertex, Vertex, Vertex)]
triangles = [ ( 1,  2,  3)
            , ( 1,  4,  5)
            , ( 2,  6,  7)
            , ( 3,  8,  9)
            , ( 4, 10, 11)
            , ( 5, 12, 13)
            , ( 6, 13, 14)
            , ( 7, 15, 16)
            , ( 8, 16, 17)
            , ( 9, 18, 10)
            , (11, 19, 20)
            , (12, 20, 21)
            , (14, 22, 23)
            , (15, 23, 24)
            , (17, 25, 26)
            , (18, 26, 27)
            , (19, 27, 28)
            , (21, 29, 22)
            , (24, 30, 25)
            , (28, 30, 29)
            ]

            
cw, ccw :: [(Color, Color, Color)]
cw  = [rgb, gbr, brg]
  where
    rgb = (R, G, B)
    gbr = (G, B, R)
    brg = (B, R, G)

ccw = [rbg, grb, bgr]
  where
    rbg = (R, B, G)
    grb = (G, R, B)
    bgr = (B, G, R)

pentagons :: [(Vertex, Vertex, Vertex, Vertex, Vertex)]
pentagons = [ (1, 5, 13, 6, 2)
            , (2, 7, 16, 8, 3)
            , (3, 9, 10, 4, 1)
            , (4, 11, 20, 12, 5)
            , (6, 14, 23, 15, 7)
            , (8, 17, 26, 18, 9)
            , (10, 18, 27, 19, 11)
            , (13, 12, 21, 22, 14)
            , (16, 15, 24, 25, 17)
            , (19, 28, 29, 21, 20)
            , (22, 29, 30, 24, 23)
            , (25, 30, 28, 27, 26)
            ]

genPentagon a b c d e = [ (a, b, c, d, e)
                        , (b, c, d, e, a)
                        , (c, d, e, a, b)
                        , (d, e, a, b, c)
                        , (e, a, b, c, d)
                        ]
                         
r0 = genPentagon R G B G B
r1 = genPentagon R B G B G
g0 = genPentagon G B R B R
g1 = genPentagon G R B R B
b0 = genPentagon B R G R G
b1 = genPentagon B G R G R


pentagonMap = M.fromList $
  [ (k, 0) | k <- r0 ] ++
  [ (k, 1) | k <- r1 ] ++
  [ (k, 2) | k <- g0 ] ++
  [ (k, 3) | k <- g1 ] ++
  [ (k, 4) | k <- b0 ] ++
  [ (k, 5) | k <- b1 ]


weight :: [Int]
weight = [10, 9, 7, 400, 3, 0]


edges :: [(Vertex, Vertex)]
edges = do (x, y, z) <- triangles
           [minmax x y, minmax y z, minmax z x]
  where
    minmax a b = (min a b, max a b)


isConsistent :: Coloring -> Bool
isConsistent color = all consistent edges
  where
    consistent (x, y) = fromMaybe True ((/=) <$> (x `M.lookup` color) <*> (y `M.lookup` color))

paint :: Coloring -> [Vertex] -> [Coloring]
paint color [] = return color
paint color (v:vs) =
  do c <- [R, G, B]
     let color' = M.insertWith (\new old -> new) v c color
     guard (isConsistent color')
     paint color' vs

initial :: Coloring
-- initial = M.fromList [(1, R), (2, G), (3, B)]

-- initial = M.empty
initial = M.fromList [(1, R), (2, B), (3, G), (5, G), (13, B), (6, G)]

paintings :: [Coloring]
paintings = paint initial (vertices L.\\ (M.keys initial))


type V = (Vertex, Color)
type E = (Vertex, Vertex, ())
type Polygon = ([V], [E])

graphParams :: G.GraphvizParams Vertex Color () () Color
graphParams = G.defaultParams
  {
    G.isDirected = False,
    G.fmtNode = \(_, c) -> case c of
      R -> color 50 123 167
      G -> color 46 137 130
      B -> color 84 186 208,
    G.fmtEdge = const []
  }
  where
    color r g b = [G.FillColor $ G.toColorList [G.RGB r g b]
                  ,G.Style [G.SItem G.Filled []]]
  

front p = length (L.filter id [ (p M.! x, p M.! y, p M.! z) `elem` cw | (x, y, z) <- triangles ])


icosadodecahedron :: Coloring -> Polygon
icosadodecahedron color = (vertices', edges')
  where
    vertices' = [(v, color M.! v) | v <- vertices]
    edges' = [(s, e, ()) | (s, e) <- L.reverse (L.drop 3 (L.reverse edges))]
histogram :: [(Vertex, Color, Int)]
histogram = [(v, c, cnt v c) | v <- vertices, c <- [R, G, B]]
  where
    cnt v c = length [ c | p <- paintings, p M.! v == c ]


pentagonHist color =
  let pens = [ (color M.! a, color M.! b, color M.! c, color M.! d, color M.! e)
             | (a, b, c, d, e) <- pentagons ]
  in [ pentagonMap M.! p | p <- pens ]


main :: IO ()
main = forM_ (L.zip [1::Int ..] paintings) $ \(i, color) -> 
  do let fileName = "icosadodecahedron-" ++ show i ++ ".dot"
         (vs, es) = icosadodecahedron color
         dotGraph = G.graphElemsToDot graphParams vs es
         dotText = G.printDotGraph dotGraph
         strText = T.toStrict dotText
     T.writeFile fileName strText
