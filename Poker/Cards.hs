module Cards where

import System.Random

data Suit = Clubs | Hearts | Spades | Diamonds deriving (Eq,Ord,Show)

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Eq,Ord)

newtype Card = Card (Value,Suit) deriving (Eq,Ord)

type Pack = [Card]

type Hand = (Card,Card,Card)

data Win = HighCard Value | Pair Value | ThreeOfAKind Value | Straight Value | Flush Suit Value | StraightFlush Suit Value deriving (Eq,Ord,Show)

dealCards :: IO (Hand,String)
dealCards = do cards <- shuffleCards
               return (hand cards,show (whatWin (hand cards)))

hand :: Pack -> Hand
hand cards = sort (take3 cards)

shuffleCards :: IO Pack
shuffleCards = shuffle initialPack

shuffle :: [a] -> IO [a]
shuffle xs = shuffle' xs (length xs)
    where shuffle' _ 0    = return []
          shuffle' xs len = do n           <- randomRIO (0, len - 1)
                               let (y, ys) =  choose n xs
                               ys'         <- shuffle' ys (len - 1)
                               return (y:ys')
          choose _ []     = error "choose: index out of range"
          choose 0 (x:xs) = (x, xs)
          choose i (x:xs) = let (y, ys) = choose (i - 1) xs in (y, x:ys)

take3 :: [a] -> (a,a,a)
take3 (c1:c2:c3:cs) = (c1,c2,c3)

sort :: Hand -> Hand
sort (c1,c2,c3) | c3 > c2 && c3 > c1 && c2 > c1 = (c1,c2,c3)
                | c3 > c2 && c3 > c1 && c1 > c2 = (c2,c1,c3)
                | c2 > c3 && c2 > c1 && c3 > c1 = (c1,c3,c2)
                | c2 > c3 && c2 > c1 && c1 > c3 = (c3,c1,c2)
                | c1 > c2 && c1 > c3 && c3 > c2 = (c2,c3,c1)
                | c1 > c2 && c1 > c3 && c2 > c3 = (c3,c2,c1)

whatWin :: Hand -> Win
whatWin (Card (vl,sl),Card (vm,sm),Card (vr,sr)) | isStraight (vl,vm,vr) = if (sl == sm && sm == sr) then StraightFlush sr (straightValue (vl,vm,vr))
                                                                                                     else Straight (straightValue (vl,vm,vr)) 
                                                 | vl == vm && vm == vr = ThreeOfAKind vr
                                                 | vl == vm = Pair vm
                                                 | vl == vr = Pair vr
                                                 | vr == vm = Pair vr
                                                 | sl == sm && sm == sr = Flush sr vr
                                                 | otherwise = HighCard vr

initialPack :: Pack
initialPack = [Card (v,s) | s <- [Clubs,Hearts,Spades,Diamonds] , v <-  [Ace,Two,Three,Four,Five,Six,Seven,Eight,Nine,Ten,Jack,Queen,King]]

isStraight :: (Value,Value,Value) -> Bool
isStraight (Two,Three,Ace) = True
isStraight (Two,Three,Four) = True
isStraight (Three,Four,Five) = True
isStraight (Four,Five,Six) = True
isStraight (Five,Six,Seven) = True
isStraight (Six,Seven,Eight) = True
isStraight (Seven,Eight,Nine) = True
isStraight (Eight,Nine,Ten) = True
isStraight (Nine,Ten,Jack) = True
isStraight (Ten,Jack,Queen) = True
isStraight (Jack,Queen,King) = True
isStraight (Queen,King,Ace) = True
isStraight (x,y,z) = False

straightValue :: (Value,Value,Value) -> Value
straightValue (Two,Three,Ace) = Three
straightValue (Two,Three,Four) = Four
straightValue (Three,Four,Five) = Five
straightValue (Four,Five,Six) = Six
straightValue (Five,Six,Seven) = Seven
straightValue (Six,Seven,Eight) = Eight
straightValue (Seven,Eight,Nine) = Nine
straightValue (Eight,Nine,Ten) = Ten
straightValue (Nine,Ten,Jack) = Jack
straightValue (Ten,Jack,Queen) = Queen
straightValue (Jack,Queen,King) = King
straightValue (Queen,King,Ace) = Ace

instance Show Value where
  show Two = " 2"
  show Three = " 3"
  show Four = " 4"
  show Five = " 5"
  show Six = " 6"
  show Seven = " 7"
  show Eight = " 8"
  show Nine = " 9"
  show Ten = "10"
  show Jack = " J"
  show Queen = " Q"
  show King = " K"
  show Ace = " A"
