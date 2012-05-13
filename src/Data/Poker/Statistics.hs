-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Poker.Statistics
-- Copyright   :  PublicDomain
--
-- Maintainer  :  lemmih@gmail.com
-- Portability :  haskell2010
--
-- Functionality for gathering information such as VPIP and PFR.
--
module Data.Poker.Statistics 
       ( historyStat
       , Stat(..)
       ) where

import Data.Poker.Parser
import Data.Poker.Eval

import Data.Ord
import Data.List
import Data.Monoid
import Text.Printf
import qualified Data.Map as Map

data ByStreet = ByStreet !Int !Int !Int !Int

instance Monoid ByStreet where
  mempty = ByStreet 0 0 0 0
  mappend (ByStreet a1 b1 c1 d1) (ByStreet a2 b2 c2 d2)
    = ByStreet (a1+a2) (b1+b2) (c1+c2) (d1+d2)

data Stat = Stat { statGames :: !Int
                 , statVPIP :: !Int
                 , statVPIPn :: !Int
                 , statShowdowns :: !Int
                 , statShownHands :: !Int
                 , statPFR :: !Int
                 , statFoldByStreet :: !ByStreet
                 , statCheckByStreet :: !ByStreet
                 , statRaiseByStreet :: !ByStreet
                 , statSeenByStreet :: !ByStreet
                 , statShowdownHands :: !(Map.Map CardMask Int)
                 }
instance Monoid Stat where
  mappend s1 s2 = Stat { statGames = statGames s1 + statGames s2
                       , statVPIP = statVPIP s1 + statVPIP s2
                       , statVPIPn = statVPIPn s1 + statVPIPn s2
                       , statShowdowns = statShowdowns s1 + statShowdowns s2
                       , statShownHands = statShownHands s1 + statShownHands s2
                       , statPFR = statPFR s1 + statPFR s2
                       , statFoldByStreet = statFoldByStreet s1 `mappend` statFoldByStreet s2
                       , statCheckByStreet = statCheckByStreet s1 `mappend` statCheckByStreet s2
                       , statRaiseByStreet = statRaiseByStreet s1 `mappend` statRaiseByStreet s2
                       , statSeenByStreet = statSeenByStreet s1 `mappend` statSeenByStreet s2
                       , statShowdownHands = Map.unionWith (+) (statShowdownHands s1) (statShowdownHands s2)
                       }
  mempty = Stat 0 0 0 0 0 0 mempty mempty mempty mempty mempty

instance Show Stat where
  show stat
    = unlines$[ printf "VPIP:        %.2f%%" vpip
              , printf "WTSD:        %.2f%% (%d of %d)" showdowns_percent showdowns nGames
              , printf "PFR:         %.2f%%" pfr_percent
              , showByStreet "Fold" (statSeenByStreet stat) (statFoldByStreet stat)
              , showByStreet "Check" (statSeenByStreet stat) (statCheckByStreet stat)
              , showByStreet "Raise" (statSeenByStreet stat) (statRaiseByStreet stat)
              , printf "Shown hands: %.2f%% (%d of %d)" shown_hands_percent shown_hands (statVPIPn stat)
              ] -- ++ popularHands shown_hands (statShowdownHands stat)
    where vpip = percent statVPIP statVPIPn
          showdowns_percent = percent statShowdowns statGames
          showdowns = statShowdowns stat
          shown_hands_percent = percent statShownHands statVPIPn
          shown_hands = statShownHands stat
          pfr_percent = percent statPFR statVPIPn
          nGames = statGames stat
          
          percent n d = fromIntegral (n stat) / fromIntegral (d stat) * 100 :: Double

popularHands totalHands hands
  = "Showdown hands:" : [ printf "  %s:  %.5f" (show hand) (fromIntegral n / fromIntegral totalHands :: Double) | (hand, n) <- sorted ]
  where sorted = take 25 $ reverse $ sortBy (comparing snd) (Map.toList hands)

showByStreet label (ByStreet seenPreflop seenFlop seenRiver seenTurn) (ByStreet preflop flop river turn)
  = concat [ printf "%s by street:\n" label
           , printf "  Preflop: %.2f%%\n" (percent preflop seenPreflop)
           , printf "  Flop:    %.2f%%\n" (percent flop seenFlop)
           , printf "  River:   %.2f%%\n" (percent river seenRiver)
           , printf "  Turn:    %.2f%%" (percent turn seenTurn)
           ]
  where percent v n = fromIntegral v / fromIntegral n * 100 :: Double

historyStat :: History -> Stat
historyStat history
  = Stat { statGames = 1
         , statVPIP = length ([ () | Bet{} <- take nPlayers (head (historyActions history)) ])
         , statVPIPn = nPlayers 
         , statShowdowns = if length (historyHands history) > 0 then 1 else 0
         , statShownHands = length (historyHands history)
         , statPFR = length ([ () | Bet n <- take nPlayers (head (historyActions history)), n > historyBB history ])
         , statFoldByStreet
             = ByStreet (length [ () | Fold <- street 0 ])
                        (length [ () | Fold <- street 1 ])
                        (length [ () | Fold <- street 2 ])
                        (length [ () | Fold <- street 3 ])
         , statCheckByStreet
             = ByStreet (length [ () | Check <- street 0 ])
                        (length [ () | Check <- street 1 ])
                        (length [ () | Check <- street 2 ])
                        (length [ () | Check <- street 3 ])
         , statRaiseByStreet
             = ByStreet (length [ () | Bet{} <- street 0 ])
                        (length [ () | Bet{} <- street 1 ])
                        (length [ () | Bet{} <- street 2 ])
                        (length [ () | Bet{} <- street 3 ])
         , statSeenByStreet
             = let preflop = nPlayers
                   flop = preflop - length [ () | Fold <- street 0 ]
                   river = flop - length [ () | Fold <- street 1 ]
                   turn = river - length [ () | Fold <- street 2 ]
               in ByStreet preflop flop river turn
         , statShowdownHands = Map.fromListWith (+) [ (cards, 1) | (_player, cards) <- historyHands history ]
         }
  where nPlayers = length (historyPlayers history)
        street n = head (drop n (historyActions history) ++ [[]])

