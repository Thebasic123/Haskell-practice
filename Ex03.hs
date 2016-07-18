module Ex03 (
  BinaryTree(..), isBST, insertBST, searchTrees,
  prop_insert_on_empty_tree, prop_insert_preserves_bst, prop_insert_adds_element, prop_insert_does_not_change_other_elements,
  prop_insert_duplicate_check,
  prop_delete_detect_error, prop_delete_preserves_bst, prop_delete_removes_element, prop_delete_does_not_change_other_elements,
  height, size
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

insertBST :: Integer -> BinaryTree -> BinaryTree
insertBST i Leaf = Branch i Leaf Leaf
insertBST i (Branch v l r) 
  | i < v = Branch v (insertBST i l) r
  | otherwise = Branch v l (insertBST i r)   

searchTrees :: Gen BinaryTree
searchTrees = sized searchTrees'
  where 
   searchTrees' 0 = return Leaf
   searchTrees' n = do 
      v <- (arbitrary :: Gen Integer)
      fmap (insertBST v) (searchTrees' $ n - 1)

checkElement :: Integer -> BinaryTree -> Bool
checkElement _ Leaf = False
checkElement x (Branch v l r) 
   | x == v = True
   | x < v = checkElement x l
   | x > v = checkElement x r


--------------


prop_insert_on_empty_tree insertFunction integer = (insertFunction integer Leaf) == Branch integer Leaf Leaf

prop_insert_preserves_bst insertFunction integer = forAll searchTrees $ \tree -> isBST (insertFunction integer tree)   

prop_insert_adds_element insertFunction integer = forAll searchTrees $ \tree -> checkElement integer (insertFunction integer tree)
 

prop_insert_does_not_change_other_elements insertFunction integer newInteger = forAll searchTrees $ \tree -> ((checkElement integer tree) == (checkElement integer (insertFunction newInteger tree)))


prop_insert_duplicate_check insertFunction integer = forAll searchTrees $ \tree -> (insertFunction integer tree) == (insertFunction integer (insertFunction integer tree))



------------

prop_delete_detect_error deleteFunction i1 i2 = 
  i1 /= i2 ==>
    deleteFunction i1 (Branch i2 Leaf Leaf) == Branch i2 Leaf Leaf 

prop_delete_preserves_bst deleteFunction integer = forAll searchTrees $ \tree -> isBST (deleteFunction integer tree)

prop_delete_removes_element deleteFunction integer = forAll searchTrees $ \tree -> not (checkElement integer (deleteFunction integer tree))

prop_delete_does_not_change_other_elements deleteFunction integer newInteger = forAll searchTrees $ \tree -> ((checkElement integer tree) == (checkElement integer (deleteFunction newInteger tree)))


----------------

height :: BinaryTree -> Integer
height Leaf = 0
height (Branch _ l r ) = if lHeight > rHeight
                             then lHeight + 1
                             else rHeight + 1
                         where lHeight = height l
                               rHeight = height r

size :: BinaryTree -> Integer
size Leaf = 0
size (Branch _ Leaf Leaf ) = 1
size (Branch _ l Leaf ) = 1 + size l
size (Branch _ l r ) = 1 + size l + size r  