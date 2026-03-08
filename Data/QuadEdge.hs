{-# LANGUAGE RankNTypes, UnicodeSyntax, FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

{- | The quad-edge data structure is commonly used in computational geometry for representing triangulations.
     It represents simultaneously both the map, its dual and mirror image.

     The fundamental idea behind the quad-edge structure is the recognition that a single edge, in a closed
     polygonal mesh topology, sits between exactly two faces and exactly two vertices. Thus, it can represent
     a dual of the graph simply by reversing the convention on what is a vertex and what is a face.

     The quad-edge data structure is described in the paper by Leonidas J. Guibas and Jorge Stolfi,
     \"Primitives for the manipulation of general subdivisions and the computation of Voronoi diagrams\",
     ACM Transactions on Graphics, 4(2), 1985, 75-123.

     This implementation is based on Stream Fusion and seems to yield similar performance to mutable implementations
     in the ST monad.
-}

module Data.QuadEdge (module Data.QuadEdge, module Data.QuadEdge.Base) where
import Data.QuadEdge.Base
import Prelude hiding (flip, lookup)
import Control.Monad.ST (ST)
import Data.Maybe

import qualified Data.Vector as IV
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as MGV
import qualified Data.QuadEdge.Stream as S

import System.Random

type QEDS  a   = IV.Vector    (Maybe (Edge a))
type MQEDS s a = MV.MVector s (Maybe (Edge a))

------------------------------------------------------------------------------
-- * Creation

-- | Create an empty QEDS

new ∷ QEDS a
new = GV.empty


------------------------------------------------------------------------------
-- * Topological modification

-- | Opens up the QEDS for in-place toplogical modification

mutate ∷ QEDS a → (forall s. MQEDS s a → ST s ()) → QEDS a
mutate q f = GV.modify f q


-- | Create a group of new edges and open up the QEDS

mutateNEs ∷ forall a. QEDS a → [a] → ([EdgeRef] → forall s. MQEDS s a → ST s ()) → (QEDS a, [EdgeRef])
mutateNEs q xs f = (GV.modify (f es) q2, es)
    where
      es :: [EdgeRef]
      es       = S.toList es'
      q2 :: QEDS a
      es' :: S.Stream EdgeRef
      (q2,es') = makeEdges q (S.fromList xs)


-- | Create a new edge and open up the QEDS

mutateNE ∷ QEDS a → a → (EdgeRef → forall s. MQEDS s a → ST s ()) → (QEDS a, EdgeRef)
mutateNE q x f = (GV.modify (f e) q2, e)
    where
      (q2,e) = makeEdge q x


-- | The QuadEdge splice operator

spliceM ∷ MQEDS s a → EdgeRef → EdgeRef → ST s ()
spliceM q a b
  | (isPrimal a && isPrimal b) || (isDual a && isDual b)
  = do oa ← onextM q a
       if b == flip oa
          then return ()
          else do
               ob ← onextM q b
               let x  = rot oa
                   y  = rot ob
               ox ← onextM q x
               oy ← onextM q y
               update a y ob
               update b x oa
               update x b oy
               update y a ox

  | otherwise = error "QuadEdge.splice: one primal and one dual edge"

  where

  update (i,r,Normal) _ n = do MGV.read q i >>= \case
                                 Nothing → undefined -- abort if working on deleted edge
                                 Just e → MGV.write q i (Just $ updET e r n)

  update (i,r,Flipped) z _  = do MGV.read q i >>= \case
                                   Nothing → undefined -- abort if working on deleted edge
                                   Just e → MGV.write q i (Just $ updET e (incrDir r) (flip z))

  updET e r v = e { edgeTable = updateET (edgeTable e) r v }



-- | Delete an edge while in mutate mode

deleteEdgeM ∷ MQEDS s a → EdgeRef → ST s ()
deleteEdgeM q e@(i,_,_) = do
                          spliceM q e =<< oprevM q e
                          spliceM q s =<< oprevM q s
                          MGV.write q i Nothing

    where s = sym e


------------------------------------------------------------------------------
-- * Edge queries

-- | Return all valid edges in the QEDS

edges ∷ QEDS a → IV.Vector (Edge a)
edges = GV.map fromJust . GV.filter isJust


-- | Return all valid edge references in the QEDS

edgerefs ∷ QEDS a → IV.Vector EdgeRef
edgerefs q = GV.unfoldr f 0
    where
      f i | i >= GV.length q = Nothing
          | isValid q r      = Just (r,i+1)
          | otherwise        = f (i+1)

          where
            r = (i,Rot0,Normal)

-- | Look up an edge. The edge must be valid.

getEdge ∷ QEDS a → EdgeRef → Edge a
getEdge q (i,_,_) = fromJust $ q GV.! i

-- | Look up the attributes of an edge. The edge must be valid.

getAttr ∷ QEDS a → EdgeRef → a
getAttr q e = attributes (getEdge q e)


-- | Return a random valid EdgeRef

randomEdgeRef ∷ RandomGen g => QEDS a → g → (EdgeRef, g)
randomEdgeRef cdt g1 =
  case cdt GV.! r of
    Just _  → ((r,Rot0,Normal),g2)
    Nothing → randomEdgeRef cdt g2

  where
    (r,g2) = randomR (0, (GV.length cdt) - 1) g1


-- | Check if an EdgeRef points to an active Edge

isValid ∷ QEDS a → EdgeRef → Bool
isValid q (i,_,_) = isJust $ q GV.! i


------------------------------------------------------------------------------
-- * Edge updating

updateEdge ∷ QEDS a → EdgeRef → Edge a → QEDS a
updateEdge q (i,_,_) e = GV.modify f q
    where
      f v = MGV.write v i (Just e)

updateAttr ∷ QEDS a → EdgeRef → a → QEDS a
updateAttr q (i,_,_) a = GV.modify f q
    where
      f v = do
            MGV.read v i >>= \case
              Nothing → undefined -- abort if working on deleted edge
              Just x → MGV.write v i (Just x{attributes=a})

------------------------------------------------------------------------------
-- * Alternate edge creation/deletion routines

-- | Delete a set of edges in one pass, using mutate and deleteEdgeM

deleteEdges ∷ QEDS a → S.Stream EdgeRef → QEDS a
deleteEdges q xs = mutate q (\v → S.mapM_ (deleteEdgeM v) xs)


makeEdges ∷ forall a. QEDS a → S.Stream a → (QEDS a, S.Stream EdgeRef)
makeEdges z xs = (qeds, S.fromList $ reverse zs)
    where
      qeds :: QEDS a
      zs :: [EdgeRef]
      (qeds,zs) = S.foldl f (z,[]) xs
          where
            f :: forall p. (QEDS p, [EdgeRef]) -> p -> (QEDS p, [EdgeRef])
            f (q,es) a = (q2,e:es)
                where
                  q2 :: QEDS p
                  e :: EdgeRef
                  (q2,e) = makeEdge q a


makeEdge ∷ QEDS a → a → (QEDS a, EdgeRef)
makeEdge q a = (q2,e)
    where
      (i,q2) = append $ \x → Edge{edgeTable=emptyET x, attributes=a}
      e      = (i, Rot0, Normal)

      append f = (index, GV.snoc q (Just $ f index))
          where
            index = GV.length q


------------------------------------------------------------------------------
-- * Adjacency Rings

-- | Returns a stream of adjacent edges using the given Adjacency Operator

ring ∷ QEDS a → (QEDS a → EdgeRef → EdgeRef) → EdgeRef → (S.Stream EdgeRef)
ring q f start@(i,_,_) = S.unfoldr g (Just start)
    where
      g :: Maybe EdgeRef -> Maybe (EdgeRef, Maybe (Index, Direction, Orientation))
      g Nothing = Nothing
      g (Just e) | j /= i    = Just (e,Just e')
                 | otherwise = Just (e, Nothing)
                 where
                   j :: Index
                   e' :: (Index,Direction,Orientation)
                   e'@(j,_,_) = f q e


------------------------------------------------------------------------------
-- * Adjacency Operators

-- | CCW around the origin

onext ∷ QEDS a → EdgeRef → EdgeRef
onext q er@(_,r,f)
    | f == Normal = lookupET r t
    | otherwise   = flip . rot $ lookupET (incrDir r) t
    where
       t = edgeTable (getEdge q er)

-- | CW around the origin

oprev ∷ QEDS a → EdgeRef → EdgeRef
oprev = rot `comp` rot

-- | CCW around the left face
lnext ∷ QEDS a → EdgeRef → EdgeRef
lnext = rot `comp` rotInv

-- | CW around the left face
lprev ∷ QEDS a → EdgeRef → EdgeRef
lprev = sym `comp` id

-- | CCW around the right face
rnext ∷ QEDS a → EdgeRef → EdgeRef
rnext = rotInv `comp` rot

-- | CW around the right face
rprev ∷ QEDS a → EdgeRef → EdgeRef
rprev = id `comp` sym

-- | CCW around the destination
dnext ∷ QEDS a → EdgeRef → EdgeRef
dnext = sym `comp` sym

-- | CW around the destination
dprev ∷ QEDS a → EdgeRef → EdgeRef
dprev = rotInv `comp` rotInv

comp ∷ (EdgeRef → a) → (b → EdgeRef) → QEDS d → b → a
comp g f qeds x = g . onext qeds $ f x

------------------------------------------------------------------------------
-- * Adjacency Operators (ST)

onextM,oprevM,lnextM,lprevM,rnextM,rprevM,dnextM,dprevM ∷ MQEDS s a → EdgeRef → ST s EdgeRef

onextM q (i, r, f) = do MGV.read q i >>= \case
                          Nothing → undefined -- abort if working on deleted edge
                          Just e → do
                            let t = edgeTable e
                            return $ if f == Normal
                                     then lookupET r t
                                     else flip (rot (lookupET (incrDir r) t))

compM ∷ (EdgeRef → b) → (t → EdgeRef) → MQEDS s a → t → ST s b
compM g f qeds x = do e ← onextM qeds (f x)
                      return (g e)

oprevM = rot `compM` rot
lnextM = rot `compM` rotInv
lprevM = sym `compM` id
rnextM = rotInv `compM` rot
rprevM = id `compM` sym
dnextM = sym `compM` sym
dprevM = rotInv `compM` rotInv

-------
