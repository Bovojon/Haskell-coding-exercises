-- Assignment 8
-- Jon

------------------ Coding Assignment 8.1: (2 Points)
data MyBinaryTree a = NullNode | Node a (MyBinaryTree a) (MyBinaryTree a)
                      deriving (Show, Eq, Ord, Read)

-- Coding Assignment 8.2: (2 Points)
leftTree :: MyBinaryTree a -> MyBinaryTree a
leftTree NullNode = NullNode
leftTree (Node _ t1 _) = t1

rightTree :: MyBinaryTree a -> MyBinaryTree a
rightTree NullNode = NullNode
rightTree (Node _ _ t2) = t2

------------------ Coding Assignment 8.3: (2 Points)
treeElem :: (Eq a) => a -> MyBinaryTree a -> Bool
treeElem a NullNode = False
treeElem a (Node b leftTree rightTree)
    | a == b    = True
    | otherwise = treeElem a leftTree || treeElem a rightTree

------------------ Coding Assignment 8.4: (4 Points)
isNullNode :: MyBinaryTree a -> Bool
isNullNode NullNode = True
isNullNode _   = False

treeVal :: MyBinaryTree a -> a
treeVal NullNode = error "There is none"
treeVal (Node v _ _ ) = v

-- These functions assume we have a binary search tree property in place
treeMax :: (Ord a) => MyBinaryTree a -> Maybe a
treeMax t
  | isNullNode t = Nothing
  | isNullNode t1 = Just v
  | otherwise = treeMax t1
    where
      t1 = rightTree t
      v = treeVal t

treeMin :: (Ord a) => MyBinaryTree a -> Maybe a
treeMin t
  | isNullNode t = Nothing
  | isNullNode t1 = Just v
  | otherwise = treeMin t1
    where
      t1 = leftTree t
      v = treeVal t

-- These functions don't assume we have a binary search tree property in place
treeMax1 :: (Ord a) => MyBinaryTree a -> a
treeMax1 t = maximum(collapseTree(t))

treeMin1 :: (Ord a) => MyBinaryTree a -> a
treeMin1 t = minimum(collapseTree(t))

------------------ Coding Assignment 8.5: (4 Points)
reflectTree :: MyBinaryTree a -> MyBinaryTree a
reflectTree NullNode = NullNode
reflectTree (Node x t1 t2) = (Node x (reflectTree t2) (reflectTree t1))

------------------ Coding Assignment 8.6: (4 Points)
collapseTree :: MyBinaryTree a -> [a]
collapseTree NullNode = []
collapseTree (Node x t1 t2) = collapseTree t1 ++ [x] ++ collapseTree t2

------------------ Coding Assignment 8.7: (4 Points)
isSorted :: (Ord a) => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) = if x <= y then isSorted (y:xs) else False

isBST :: (Ord a) => MyBinaryTree a -> Bool
isBST tree = isSorted(collapseTree tree)

------------------ Coding Assignment 8.8: (4 Points)
bstAdd :: (Ord a) => a -> MyBinaryTree a -> MyBinaryTree a
bstAdd val NullNode = (Node val NullNode NullNode)
bstAdd val (Node v t1 t2)
  | isBST (Node v t1 t2) == False = (Node v t1 t2)
  | v == val = Node v t1 t2
  | val > v  = Node v t1 (bstAdd val t2)
  | val < v  = Node v (bstAdd val t1) t2

------------------ Coding Assignment 8.9: (4 Points)
listToBST :: Ord a => [a] -> MyBinaryTree a
listToBST (x:xs)
   | length xs == 0 = bstAdd x NullNode
   | otherwise = bstAdd x (listToBST xs)

treeToBST :: Ord a => MyBinaryTree a -> MyBinaryTree a
treeToBST tree = listToBST (collapseTree tree)

------------------ Coding Assignment 8.10: (6 Points)
miniTree :: Ord a => MyBinaryTree a -> Maybe a
miniTree t
   | isNullNode t = Nothing
   | isNullNode t1 = Just v
   | otherwise = miniTree t1
       where
           t1 = leftTree t
           v = treeVal t

join :: (Ord a) => MyBinaryTree a -> MyBinaryTree a -> MyBinaryTree a
join t1 t2 = Node mini t1 newt
              where
                (Just mini) = miniTree t2
                newt        = delFromBST mini t2

delFromBST :: (Ord a) => a -> MyBinaryTree a -> MyBinaryTree a
delFromBST val (Node v t1 t2)
  | isBST (Node v t1 t2) == False = (Node v t1 t2)
  | (treeElem val (Node v t1 t2)) == False = (Node v t1 t2)
  | val < v = Node v (delFromBST val t1) t2
  | val > v = Node v t1 (delFromBST val t2)
  | isNullNode t2 = t1
  | isNullNode t1 = t2
  | otherwise = join t1 t2
  
  
------------------ Coding Assignment 8.11: (4 Points)
binaryLookup :: (Ord a) => a -> MyBinaryTree a -> Bool
binaryLookup a NullNode = False
binaryLookup a (Node b leftTree rightTree)
    | a == b    = True
    | a > b     = binaryLookup a rightTree
    | a < b     = binaryLookup a leftTree
 
------------------ Coding Assignment 8.12: (10 Points)
-- This structure is a Queue that is a FIFO structure.

data Queue a = Queue [a] deriving (Show)

-- Functions that facilitate basic operations with your data structure
emptyQ :: Queue a
emptyQ = Queue []

isEmpty :: Queue a -> Bool
isEmpty (Queue []) = True
isEmpty _             = False

addQ :: a -> Queue a -> Queue a
addQ x (Queue xs) = Queue (xs++[x])

peek :: Queue a -> a
peek (Queue (x:xs)) = x

remQ :: Queue a -> (a, Queue a)
remQ q@(Queue xs)
  | not (isEmpty q) = (head xs, Queue (tail xs))
  | otherwise = error "remQ"
 
-- Functions that use the queue to manage process IDs which are handled by FIFO algorithm
-- so the one that is added to the queue first is executed first
addProcessID :: Int -> Queue Int -> Queue Int
addProcessID int queue = addQ int queue

executeProcessID :: Int -> Queue Int -> (Int, Queue Int)
executeProcessID int queue = remQ queue
 
---------------------------------------------------------------- References
-- Haskell-The-Craft-of-Functional-Programming
-- https://rafal.io/posts/haskell-queues.html
-- https://stackoverflow.com/questions/22122898/checking-if-an-element-exists-in-a-tree
-- https://gist.github.com/kaveet/b77f2f3add61d9c3afdb6852b7f36b03
