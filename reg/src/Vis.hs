module Vis where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text

import Data.Set (Set)
import qualified Data.Set as Set

import Data.GraphViz.Commands hiding (Eps)
import Data.GraphViz.Types.Canonical
import Data.GraphViz.Types
import Data.GraphViz.Attributes
import Data.GraphViz.Printing

import Automata
import TrSys

tsToDot :: (Ord a, Show a) => TrSys a -> Set a -> DotGraph Text
tsToDot tr ss =
  DotGraph {
    strictGraph = False,
    directedGraph = True,
    graphID = Nothing,
    graphStatements =
      DotStmts {
        attrStmts = [],
        subGraphs = [],
        nodeStmts = [],
        edgeStmts = edges
      }
  }
  where
    txt = Text.pack . show
    mkEdge s t =
      if Set.member t ss then [DotEdge (txt s) (txt t) []] else []
    addEdges s acc = Set.foldr (\t es ->  mkEdge s t ++ es) acc (tr s)
    edges = Set.foldr addEdges [] ss

instance (PrintDot a) => PrintDot (Maybe a) where
  unqtDot Nothing  = text (Text.pack "_")
  unqtDot (Just x) = unqtDot x

autoToDot :: (Show a, Show q) => Auto a q -> DotGraph (Maybe Text)
autoToDot m =
  DotGraph {
    strictGraph = False,
    directedGraph = True,
    graphID = Nothing,
    graphStatements =
      DotStmts {
        attrStmts = [NodeAttrs [shape Circle], EdgeAttrs [arrowTo vee]],
        subGraphs = [],
        nodeStmts = DotNode Nothing [style invis] : nodes,
        edgeStmts = DotEdge Nothing (txt (start m)) [] : edges
      }
  }
  where
    txt = Just . Text.pack . show
    mkEdge (p,S a,q) = DotEdge (txt p) (txt q) [toLabel (show a)]
    mkEdge (p,E  ,q) = DotEdge (txt p) (txt q) []
    edges = map mkEdge (trans m)
    nodes = map (\q -> DotNode (txt q) [shape DoubleCircle]) (final m)

vizAuto :: (Show a, Show q) => Auto a q -> FilePath -> IO FilePath
vizAuto a = runGraphviz (autoToDot a) Png

vizTrSys :: (Ord a, Show a) => TrSys a -> Set a -> FilePath -> IO FilePath
vizTrSys tr ss = runGraphviz (tsToDot tr ss) Png

