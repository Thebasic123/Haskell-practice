module Ex09 (BinaryTree(..), treeMap, concTreeMap) where

import Control.Concurrent
import System.Random 
import Control.Applicative
import System.Environment

data BinaryTree a = Leaf | Branch a (BinaryTree a) (BinaryTree a) deriving (Read, Show, Eq)

treeMap :: (a -> b) -> BinaryTree a -> BinaryTree b
treeMap f Leaf = Leaf
treeMap f (Branch v l r) = let v' = f v
                               l' = treeMap f l 
                               r' = treeMap f r
                           in
                           Branch v' l' r'

concTreeMap :: (a -> b) -> BinaryTree a -> IO (BinaryTree b)
concTreeMap f Leaf = return Leaf
concTreeMap f (Branch v l r) = do
  let v' = f v
  l' <- concTreeMap f l
  r' <- concTreeMap f r
  return (Branch v' l' r')





--concTreeMap :: (a -> b) -> BinaryTree a -> IO (BinaryTree b)
--concTreeMap f Leaf = return Leaf
--concTreeMap f (Branch v l r) = do
--  m <- newEmptyMVar
--  forkIO $ do
--          seq (f v) (putMVar m (f v))
--  l' <- concTreeMap f l
--  r' <- concTreeMap f r
--  v' <- takeMVar m
--  return (Branch v' l' r')






--concTreeMap :: (a -> b) -> BinaryTree a -> IO (BinaryTree b)
--concTreeMap f Leaf = return Leaf
--concTreeMap f (Branch v l r) = do
--m <- newEmptyMVar
--let forkIO $ do
--        seq (f v) (putMVar m (f v))
--	v' <- takeMVar m 
--    l' = treeMap f l
--    r' = treeMap f r
--return (Branch v' l' r')