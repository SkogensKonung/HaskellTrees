data BST a = Nil | Node a (BST a) (BST a)
 deriving (Show,Eq,Ord)

insert::(Ord a)=> a -> BST a -> BST a
insert k Nil = Node k Nil Nil
insert k (Node x left right)
 | k>=x = Node x left (insert k right)
 | otherwise = Node x (insert k left) right

findMin::(Ord a) => BST a -> a
findMin (Node x Nil _) = x
findMin (Node x left right) = findMin left

findMax::(Ord a) => BST a -> a
findMax (Node x _ Nil) = x
findMax (Node x left right) = findMax right
