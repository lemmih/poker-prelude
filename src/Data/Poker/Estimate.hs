module Data.Poker.Estimate where

import Data.Poker.Eval

import Data.List
import Data.Bits
import Data.Char
import Text.Printf
import Foreign.C

data Hand = StraightFlush Rank
          | FourOfAKind Rank
          | FullHouse Rank
          | Flush Rank
          | Straight Rank
          | ThreeOfAKind Rank
          | TwoPair Rank
          | OnePair Rank
          | HighCard Rank
    deriving (Show)

data StartingHand = Pair Rank
                  | Suited Rank Rank
                  | Offsuit Rank Rank
    deriving (Show)

parseRange :: String -> [StartingHand]
parseRange inp
    = concatMap parseToken tokens
    where tokens = words inp
          parseToken [r1,r2] | r1==r2
              = [ Pair (parseRank r1) ]
          parseToken [r1,r2]
              = [ Suited (parseRank r1) (parseRank r2) ]
          parseToken [r1,r2,'o']
              = [ Offsuit (parseRank r1) (parseRank r2) ]
          parseToken [r1,r2,'+']
              = let rn1 = parseRank r1; rn2 = parseRank r2
                in if r1 == r2 then [ Pair rank | rank <- [rn1 .. Ace]] else [ Suited rn1 rank | rank <- init [rn2 .. rn1]]
          parseToken [r1,r2,'o','+']
              = let rn1 = parseRank r1; rn2 = parseRank r2
                in [ Offsuit rn1 rank | rank <- init [rn2 .. rn1]]
          parseRank c = case lookup c ([ (intToDigit n, toEnum (n-2)) | n <- [2..9] ] ++ [ (head (show rank), rank)| rank <- [Ten .. Ace]]) of
              Nothing -> error $ "Invalid rank: " ++ show c
              Just rank -> rank

allStartingHands = [Pair rank | rank <- [Two .. ]] ++
                   [Suited r1 r2 | r1 <- [Three .. ], r2 <- init [Two .. r1]] ++
                   [Offsuit r1 r2 | r1 <- [Three .. ], r2 <- init [Two .. r1]]

startingHandLikelihood :: StartingHand -> Double
startingHandLikelihood Pair{} = 6 / ((52*51)/2) -- estOnePair emptyMask Ace 2
startingHandLikelihood Suited{} = 4 / ((52*51)/2)
startingHandLikelihood Offsuit{} = 12 / ((52*51)/2)


handStrength :: StartingHand -> Int -> String
handStrength startingHand opponents
    = let win = sum [ sumAttackList (handAttackList startingHand) (handAttackList opponentHand) * startingHandLikelihood opponentHand
                      | opponentHand <- allStartingHands ]
          lose = sum [ sumAttackList (handAttackList opponentHand) (handAttackList startingHand) * startingHandLikelihood opponentHand
                       | opponentHand <- allStartingHands ]
          tie = 1-win-lose
      in printf "Win: %.2f%%, lose: %.2f%%, tie: %.2f%%" (win*100) (lose*100) (tie*100)

handAttackList :: StartingHand -> [Double]
handAttackList hand
    = normalise $ attackList mask
    where mask = case hand of
                   Pair rank     -> maskFromCards [ mkCard rank Hearts, mkCard rank Clubs ]
                   Suited r1 r2  -> maskFromCards [ mkCard r1 Hearts, mkCard r2 Hearts ]
                   Offsuit r1 r2 -> maskFromCards [ mkCard r1 Hearts, mkCard r2 Clubs ]


{-
[0.2,0.7,0.1]
[0.7,0.2,0.1]

0.7*(0.7+0.1) + 0.2*0.1 = 0.58 -- win
0.7*0.2 + 0.2*0.7 + 0.1*0.1 = 0.29 -- tie


0.2*(0.2+0.1) + 0.7*0.1 = 0.13


[0.5,0.5,0.5]
[0.5,0.25,0.125]
-}
normalise :: [Double] -> [Double]
normalise = worker 1
    where worker acc [] = []
          worker acc (x:xs) = acc * x : worker (acc - acc*x) xs

sumAttackList :: [Double] -> [Double] -> Double
sumAttackList a b
    = sum [ elt * (sum lower)
            | (elt, (equal:lower)) <- zip a (tails b)]

--cmpAttackList :: Fractional a => [a] -> [a] -> a
cmpAttackList [] [] = 0
cmpAttackList (x:xs) (y:ys)
    = let win = x * (1-y)
          neither = (1-x) * (1-y)
      in win + neither*cmpAttackList xs ys
          

attackList :: CardMask -> [Double]
attackList mask
    = reverse $ [ estHighCard mask rank 5 | rank <- [Two .. Ace]] ++
                [ estOnePair mask rank 5 | rank <- [Two .. Ace]] ++
                [ estTwoPair mask rank 5 | rank <- [Two .. Ace]] ++
                [ estThreeOfAKind mask rank 5 | rank <- [Two .. Ace]]

{-

cards


change of getting one pair with n cards left.

high card * 13
one pair * 13
two pair * 13
straight * 13
flush * 13
four of a kind * 13
straight flush * 13

-}

--double est_highcard_n(int rank, StdDeck_CardMask cards, int picks)
foreign import ccall unsafe "est_highcard_n" c_est_highcard_n :: CInt -> StdDeck_CardMask -> CInt -> CDouble
estHighCard :: CardMask -> Rank -> Int -> Double
estHighCard mask rank picks
    = realToFrac (c_est_highcard_n (rankToCInt rank) (unmask mask) (fromIntegral picks))

--double est_onepair_n(int rank, StdDeck_CardMask cards, int picks)
foreign import ccall unsafe "est_onepair_n" c_est_onepair_n :: CInt -> StdDeck_CardMask -> CInt -> CDouble

estOnePair :: CardMask -> Rank -> Int -> Double
estOnePair mask rank picks
    = realToFrac (c_est_onepair_n (rankToCInt rank) (unmask mask) (fromIntegral picks))

--double est_twopair_n(int rank, StdDeck_CardMask cards, int picks)
foreign import ccall unsafe "est_twopair_n" c_est_twopair_n :: CInt -> StdDeck_CardMask -> CInt -> CDouble

estTwoPair :: CardMask -> Rank -> Int -> Double
estTwoPair mask rank picks
    = realToFrac (c_est_twopair_n (rankToCInt rank) (unmask mask) (fromIntegral picks))


foreign import ccall unsafe "est_threeofakind_n" c_est_threeofakind_n :: CInt -> StdDeck_CardMask -> CInt -> CDouble

estThreeOfAKind :: CardMask -> Rank -> Int -> Double
estThreeOfAKind mask rank picks
    = realToFrac (c_est_threeofakind_n (rankToCInt rank) (unmask mask) (fromIntegral picks))




{-

masks for high card: 4
mask for one pair: 6
mask for two pair: 6*6.5*6
mask for three of a kind: 4
-}






{-

|-|
|---|

|--|
|--|
chanceOfPickingRight * chanceOfPickingRight

|--|
|-|
0

|-|
|--|
1 - (chanceOfNotPicking * chanceOfNotPicking)

|--|
|---|
  |-|
  |--|
  chanceOfPicking * ^^
  
  |--|
  |--|
  chanceOfNotPicking * chanceOfPickingRight * chanceOfPickingRight


-}
prop deck wanted _ | wanted <= 0
    = 1
prop deck wanted picks | wanted == picks
    = 1/deck * prop (deck-1) (wanted-1) (picks-1)
prop deck wanted picks | wanted < picks
    = 1/deck * prop (deck-1) (wanted-1) (picks-1) +
      (deck-1)/deck * prop (deck-1) wanted (picks-1)
prop deck wanted picks | wanted > picks
    = 0

prop' deck [] _
    = 1
prop' deck (w:ws) 0
    = 0
prop' deck (w:ws) picks 
    = w/deck * prop' (deck-1) ws (picks-1) +
      (deck-w)/deck * prop' (deck-1) (w:ws) (picks-1)


orProps props
    = 1 - product [ 1 - prop | prop <- props ]
chanceOfMatch cardMask picks nCards orMasks
    = orProps
      [ let dist = numberOfCards (or `subtractCards` cardMask)
        in prop nCards dist picks
        | or <- orMasks ]

CardMask a `subtractCards` CardMask b
    = CardMask (a .&. (complement b))

{-
handProps cardMask picks nCards
    = worker [] (reverse hands)
    where worker deadMasks [] = []
          worker deadMasks ((orMasks, hand) : xs)
              = (orProps
                [ let dist = numberOfCards (orMask `subtractCards` cardMask)
                  in prop nCards dist picks | orMask <- orMasks, (orMask `maskOr` cardMask) `notIn` deadMasks ]
                , hand)
                : worker (deadMasks ++ orMasks) xs
-}
notIn :: CardMask -> [CardMask] -> Bool
x `notIn` []     = True
x `notIn` (y:ys) = numberOfCards (y `subtractCards` x) /= 0 && x `notIn` ys


hands :: [([CardMask], Hand)]
hands = concat [ mk highHandMasks HighCard
               , mk onePairMasks OnePair
               , mk threeOfAKindMasks ThreeOfAKind ]
    where mk fn hand = [ (fn rank, hand rank) | rank <- [minBound .. maxBound] ]

handProps cardMask picks nCards
    = worker [] (reverse handPropList)
    where worker deadMasks [] = []
          worker deadMasks ((propFn, hand) : xs)
              = (propFn cardMask picks nCards, hand)
                : worker deadMasks xs

handPropList :: Fractional a => [(CardMask -> Int -> a -> a, Hand)]
handPropList
    = concat [ mk highHandProp HighCard,
               mk onePairProp OnePair,
               mk twoPairProp TwoPair,
               mk threeOfAKindProp ThreeOfAKind,
               mk straightProp Straight,
               mk flushProp Flush,
               mk fourOfAKindProp FourOfAKind]
    where mk fn hand
              = [ (fn rank, hand rank) | rank <- allRanks ]

allRanks :: [Rank]
allRanks = [minBound .. maxBound]

propFromHands hands cardMask picks nCards
    = orProps
      [ let dist = numberOfCards (orMask `subtractCards` cardMask)
        in prop nCards dist picks
        | orMask <- hands ] :: Rational

highHandProp rank cardMask picks nCards
    = prop' nCards (drop (countRank cardMask rank) [4]) picks
onePairProp rank cardMask picks nCards
    = prop' nCards (drop (countRank cardMask rank) [4,3]) picks
twoPairProp rank cardMask picks nCards
    = let missing = max 0 (2 - countRank cardMask rank)
          otherPairProp = orProps [ onePairProp lowRank cardMask (picks-missing) (nCards-fromIntegral missing) | lowRank <- lowerRanks ]
      in prop' nCards (drop (2 - missing) [4,3]) 2 * otherPairProp
    where lowerRanks = init [ minBound .. rank ]
threeOfAKindProp rank cardMask picks nCards
    = prop' nCards (drop (countRank cardMask rank) [4,3,2]) picks
fourOfAKindProp rank cardMask picks nCards
    = prop' nCards (drop (countRank cardMask rank) [4,3,2,1]) picks
straightProp Four _ _ _ = 0
straightProp Three _ _ _ = 0
straightProp Two _ _ _ = 0
straightProp rank cardMask picks nCards
    = prop' nCards c picks
    where c = concat [ if countRank cardMask r == 0 then [4] else [] | r <- lineup ]
          lineup = if rank == Five then Ace : [Two .. Five]
                   else take 5 (reverse [Two .. rank])
flushProp rank cardMask picks nCards
    = orProps
      [ if hasCard cardMask (mkCard rank suit)
        then prop' nCards (drop suited [13,12,11,10,9]) picks
        else (13-fromIntegral suited)/nCards * prop' (nCards-1) (drop (suited+1) [13,12,11,10,9]) (picks-1)
       | suit <- [minBound .. maxBound]
      , let suited = (numberOfCards (cardMask `maskAnd` suitMask suit)) ]
    where suitMask suit = maskFromCards [ mkCard r suit | r <- [minBound .. rank] ]


highHandMasks rank
    = [ maskFromCards [ mkCard rank suit ]
        | suit <- [minBound .. maxBound ]]

onePairMasks rank
    = [ maskFromCards [ mkCard rank suit1, mkCard rank suit2 ]
        | suit1 <- [minBound .. maxBound]
      , suit2 <- [minBound .. maxBound]
      , suit1 /= suit2 ]

threeOfAKindMasks rank
    = [ maskFromCards [ mkCard rank suit1, mkCard rank suit2, mkCard rank suit3 ]
       | suit1 <- [minBound .. maxBound]
      , suit2 <- [minBound .. maxBound]
      , suit3 <- [minBound .. maxBound]
      , suit1 /= suit2
      , suit1 /= suit3
      , suit2 /= suit3 ]











