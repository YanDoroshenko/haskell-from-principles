module BinaryTree where

data BinaryTree a =
  Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a  = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node l a r) = a : (preorder l) ++ (preorder r)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node l a r) = (inorder l) ++ a : (inorder r)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node l a r) = (postorder l) ++ (postorder r) ++ [a]

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ z Leaf = z
foldTree f z (Node l a r) = foldTree f z'' r
  where z'' = f a z'
        z' = foldTree f z l

testTree :: BinaryTree Integer
testTree =
  Node (Node Leaf 1 Leaf)
  2
  (Node Leaf 3 Leaf)

