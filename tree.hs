data Bit = T | F deriving Show
data Tree = Leaf | Node Tree Tree deriving Show

makeTree::Int->Tree
makeTree 0 = Leaf
makeTree n = Node (makeTree (n-1)) (makeTree (n-1))

countNodes::Tree->Int
countNodes Leaf = 1
countNodes (Node left right) = 1 + countNodes left + countNodes right

height::Tree->Int
height Leaf = 0
height (Node left right) = 1 + (max (height left) (height right))

-- verbesserung erfordert?
bal::Tree->Bool
bal Leaf = True
bal (Node l r) = bal l && bal r && (height l == height r)

insertLeaf::Tree->Tree
insertLeaf Leaf = (Node Leaf Leaf)
insertLeaf (Node l r) =
 if (height l > height r)
 then (Node l (insertLeaf r))
 else (Node (insertLeaf l) r)

deleteLeaf::Tree->Tree
deleteLeaf (Node Leaf Leaf) = Leaf
deleteLeaf (Node l r) =
 if (height l > height r)
 then (Node (deleteLeaf l) r)
 else (Node l (deleteLeaf r))

-- pfaflaenge
-- Summe der Tiefen aller inneren Knoten

pfadlaenge::Tree->Int
pfadlaenge Leaf = 0
pfadlaenge (Node l r) = pfadlaenge l + pfadlaenge r + (countNodes (Node l r)) - 1






