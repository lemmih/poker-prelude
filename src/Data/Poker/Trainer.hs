module Data.Poker.Trainer where

import Data.Poker.Eval

import Data.Ratio
import System.Random
import System.IO
import Text.Printf


{- Count outs:

Community: _ _ _ ?

Opponent: _ _

You: _ _

-}

pickN :: CardMask -> Int -> IO [Card]
pickN deadCards 0 = return []
pickN deadCards n
    = do card <- randomIO
         if deadCards `hasCard` card then pickN deadCards n
           else do rest <- pickN (deadCards `maskOr` maskFromCards [card]) (n-1)
                   return (card:rest)

trainerCountOuts :: IO ()
trainerCountOuts
    = do hSetBuffering stdout NoBuffering
         community <- fmap maskFromCards $ pickN emptyMask 4
         [c1,c2,c3,c4] <- pickN community 4
         let hero = maskFromCards [c1,c2]
             villain = maskFromCards [c3,c4]
             outs = countOuts community hero villain
             ins = countOuts community villain hero
         printf "Community: %s\n" (maskToString community)
         printf "Villain:   %s\n" (maskToString villain)
         printf "Hero:      %s\n" (maskToString hero)
         putStr "Outs: "
         inp <- getLine
         if inp == "q" then return ()
           else do printf "Answer: %d (%d)\n" (length outs) (length ins)
                   print $ map maskToString outs
                   putStrLn "\n"
                   trainerCountOuts


countOuts :: CardMask -> CardMask -> CardMask -> [CardMask]
countOuts community hero villain
    = 
      [ pair | pair <- validPairs,
        let heroHand = community `maskOr` hero `maskOr` pair
            villainHand = community `maskOr` villain `maskOr` pair
        , evalHand heroHand > evalHand villainHand ]
    where allCards = [ mkCard rank suit | rank <- [minBound .. maxBound], suit <- [minBound .. maxBound]]
          pairs = if numberOfCards community == 3
                  then [ maskFromCards [h1, h2] | h1 <- allCards, h2 <- allCards ]
                  else [ maskFromCards [h1] | h1 <- allCards ]
          validPairs = [ pair | pair <- pairs, numberOfCards (deadCards `maskOr` pair) == 9 ]
          deadCards = community `maskOr` hero `maskOr` villain


