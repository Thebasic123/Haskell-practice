module Ex02 (
  BinaryTree(..), isBST, insert, deleteAll, searchTrees, isSorted, sortedListsWithoutDuplicates, isBalanced,
  mysteryPred, mysterious, astonishing
) where

import Test.QuickCheck
import Data.List(sort, nub)

data BinaryTree = Branch Integer BinaryTree BinaryTree
                | Leaf
                deriving (Show, Ord, Eq)

isBST :: BinaryTree -> Bool
isBST Leaf = True
isBST (Branch v l r) = allTree (< v) l && allTree (>= v) r && isBST l && isBST r
  where allTree :: (Integer -> Bool) -> BinaryTree -> Bool
        allTree f (Branch v l r) = f v && allTree f l && allTree f r
        allTree f (Leaf) = True
        
--Add an integer to a BinaryTree, preserving BST property.
insert :: Integer -> BinaryTree -> BinaryTree
insert i Leaf = Branch i Leaf Leaf
insert i (Branch v l r) 
  | i < v = Branch v (insert i l) r
  | otherwise = Branch v l (insert i r)

--Remove all instances of an integer in a binary tree, preserving BST property
deleteAll :: Integer -> BinaryTree -> BinaryTree
deleteAll i Leaf = Leaf
deleteAll i (Branch j Leaf r) | i == j = deleteAll i r
deleteAll i (Branch j l Leaf) | i == j = deleteAll i l
deleteAll i (Branch j l r) | i == j = let (x, l') = deleteRightmost l
                                       in Branch x l' (deleteAll i r)
                           | i <  j = Branch j (deleteAll i l) r
                           | i >  j = Branch j l (deleteAll i r)
  where deleteRightmost :: BinaryTree -> (Integer, BinaryTree)
        deleteRightmost (Branch i l Leaf) = (i, l)
        deleteRightmost (Branch i l r)    = let (x, r') = deleteRightmost r
                                             in (x, Branch i l r')

searchTrees :: Gen BinaryTree
searchTrees = sized searchTrees'
  where 
   searchTrees' 0 = return Leaf
   searchTrees' n = do 
      v <- (arbitrary :: Gen Integer)
      fmap (insert v) (searchTrees' $ n - 1)

----------------------

mysteryPred :: Integer -> BinaryTree -> Bool
mysteryPred _ Leaf = False
mysteryPred x (Branch v l r) 
   | x == v = True
   | x < v = mysteryPred x l
   | x > v = mysteryPred x r


----------------------

mysterious :: BinaryTree -> [Integer]
mysterious Leaf = []
mysterious (Branch v l r) = sort ([v] ++ mysterious l ++ mysterious r)

isSorted :: [Integer] -> Bool
isSorted (x:y:rest) = x <= y && isSorted (y:rest)
isSorted _ = True

----------------------

-- Note `nub` is a function that removes duplicates from a sorted list
sortedListsWithoutDuplicates :: Gen [Integer]
sortedListsWithoutDuplicates = fmap (nub . sort) arbitrary

astonishing :: [Integer] -> BinaryTree
astonishing [] = Leaf
astonishing [x] = Branch x Leaf Leaf
astonishing array = Branch v (astonishing l) (astonishing r)
   where v = array !! (getMiddle array - 1)
         l = take (getMiddle array - 1) array
         r = drop (getMiddle array) array

getMiddle :: [Integer] -> Int
getMiddle array = if len `mod` 2 == 0 
                      then (quot len 2) 
                      else (quot len 2) + 1
                where len = length(array)

isBalanced :: BinaryTree -> Bool
isBalanced Leaf = True
isBalanced (Branch v l r) = abs (height l - height r) <= 1 && isBalanced l && isBalanced r
  where height Leaf = 0
        height (Branch v l r) = 1 + max (height l) (height r)