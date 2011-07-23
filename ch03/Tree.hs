-- file: ch03/Tree.hs

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

simpleTree = Node "parent" (Node "left child" Empty Empty)
                           (Node "right child" Empty Empty)

data Tree' a = Node' a
                     (Maybe (Tree' a)) 
                     (Maybe (Tree' a))
                     deriving (Show)

simpleTree' = Node' "parent" (Just (Node' ("left-child") Nothing Nothing)) Nothing
