module PrefixTree where

import Prelude hiding (lookup)
import Char

type Key a = [a]

alex :: Key Char
alex = "alex"

data PFTree a b = Node (Maybe b) (a -> Maybe (PFTree a b))

initialTree :: PFTree a b
initialTree = Node Nothing (\_ -> Nothing)

lookup :: Key a -> PFTree a b -> Maybe b
lookup [] (Node b _) = b
lookup (x:xs) (Node b f) = case f x of
		                Nothing -> Nothing
			        Just pt -> lookup xs pt

insert :: Eq a => Key a -> b -> PFTree a b -> PFTree a b
insert [] b (Node _ f) = Node (Just b) f
insert (x:xs) b (Node mb f) = case f x of
                                   Nothing -> Node mb (\x' -> if x' == x then Just (insert xs b initialTree) 
                                                                         else f x')
				   Just pt -> Node mb (\x' -> if x' == x then Just (insert xs b pt) 
                                                                         else f x')


nodesTo :: Key a -> PFTree a b -> [Maybe b]
nodesTo [] (Node b _) = [b]
nodesTo (x:xs) (Node b f) = case f x of
	    	                 Nothing -> [b]
			         Just pft -> b : nodesTo xs pft


fromList :: Eq a => [(Key a,b)] -> PFTree a b
fromList [] = initialTree
fromList ((ka,b):xs) = insert ka b (fromList xs)

testTree :: PFTree Char Int
testTree = fromList [("Alex",4),("Alexander",9),("Karen",5),("Karen Thur",10),("Alex Green",10),("Alexander Green",15),
                     ("A. Green",8),("alex",4),("alexander",9),("Karen Elizabeth Thur",20),("Karen Elizabeth",15),
                     ("Al",2),("al",2)]


--display :: Show b -> PFTree Char b -> String
--display (Node Nothing f) = ""
--display (Node (Just b) f) = show b 

--display' :: Show b -> Char -> PFTree Char b -> String
--display' c (Node Nothing f) 


  










