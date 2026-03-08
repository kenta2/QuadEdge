{-# LANGUAGE FlexibleInstances, Rank2Types, BangPatterns #-}

-- |
-- Module      : Data.Vector.Fusion.Stream
-- Copyright   : (c) Roman Leshchinskiy 2008-2010
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
-- 
-- Streams for stream fusion
--

module Data.Vector.Fusion.Stream (
  -- * Types
  Stream,

  -- * Folding
  foldl,

  -- * Unfolding
  unfoldr,

  -- * Conversions
  toList, fromList, liftStream,

  -- * Monadic combinators
  mapM_
) where

import Data.Vector.Fusion.Util
import Data.Vector.Fusion.Stream.Monadic ( Step(..) )
import qualified Data.Vector.Fusion.Stream.Monadic as M

import Prelude hiding ( length, null,
                        replicate, (++),
                        head, last, (!!),
                        init, tail, take, drop,
                        map, concatMap,
                        zipWith, zipWith3, zip, zip3,
                        filter, takeWhile, dropWhile,
                        elem, notElem,
                        foldl, foldl1, foldr, foldr1,
                        and, or,
                        scanl, scanl1,
                        enumFromTo, enumFromThenTo,
                        mapM, mapM_ )

import GHC.Base ( build )

#include "vector.h"

-- | The type of pure streams 
type Stream = M.Stream Id

-- | Convert a pure stream to a monadic stream
liftStream :: Monad m => Stream a -> M.Stream m a
{-# INLINE_STREAM liftStream #-}
liftStream (M.Stream step s sz) = M.Stream (return . unId . step) s sz

-- Folding
-- -------

-- | Left fold
foldl :: (a -> b -> a) -> a -> Stream b -> a
{-# INLINE foldl #-}
foldl f z = unId . M.foldl f z

-- Unfolding
-- ---------

-- | Unfold
unfoldr :: (s -> Maybe (a, s)) -> s -> Stream a
{-# INLINE unfoldr #-}
unfoldr = M.unfoldr

-- Monadic combinators
-- -------------------

-- | Apply a monadic action to each element of the stream
mapM_ :: Monad m => (a -> m b) -> Stream a -> m ()
{-# INLINE mapM_ #-}
mapM_ f = M.mapM_ f . liftStream

-- Conversions
-- -----------

-- | Convert a 'Stream' to a list
toList :: Stream a -> [a]
{-# INLINE toList #-}
-- toList s = unId (M.toList s)
toList s = build (\c n -> toListFB c n s)

-- This supports foldr/build list fusion that GHC implements
toListFB :: (a -> b -> b) -> b -> Stream a -> b
{-# INLINE [0] toListFB #-}
toListFB c n (M.Stream step s _) = go s
  where
    go s = case unId (step s) of
             Yield x s' -> x `c` go s'
             Skip    s' -> go s'
             Done       -> n

-- | Create a 'Stream' from a list
fromList :: [a] -> Stream a
{-# INLINE fromList #-}
fromList = M.fromList
