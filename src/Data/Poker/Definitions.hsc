module Data.Poker.Definitions where

import Foreign.C
import Foreign.Storable
import Foreign.Marshal
import Foreign.Ptr
import Data.Word
import Data.List

#include "poker_defs.h"
#include "enumdefs.h"
#include "inlines/eval.h"

type StdDeck_CardMask = CLLong

data EnumResult
    = EnumResult { resultSamples :: Integer
                 , resultPlayers :: [PlayerResult] }

data PlayerResult
    = PlayerResult { playerWins :: Integer
                   , playerTies :: Integer
                   , playerLosses :: Integer
                   , playerEV   :: Double }

instance Storable EnumResult where
    sizeOf _ = #{size enum_result_t}
    alignment _ = 4
    peek ptr = do samples <- #{peek enum_result_t, nsamples} ptr :: IO CUInt
                  c_players <- #{peek enum_result_t, nplayers} ptr :: IO CUInt
                  let players = fromIntegral c_players
                  win <- peekArray players (#{ptr enum_result_t, nwinhi} ptr) :: IO [CUInt]
                  tie <- peekArray players (#{ptr enum_result_t, ntiehi} ptr) :: IO [CUInt]
                  lose <- peekArray players (#{ptr enum_result_t, nlosehi} ptr) :: IO [CUInt]
                  ev <- peekArray players (#{ptr enum_result_t, ev} ptr) :: IO [CDouble]
                  let mkPlayer a b c d = PlayerResult (fromIntegral a) (fromIntegral b) (fromIntegral c) (realToFrac d)
                  return EnumResult{ resultSamples = fromIntegral samples
                                   , resultPlayers = zipWith4 mkPlayer win tie lose ev }

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
          | Jack | Queen | King | Ace deriving (Show,Eq,Ord,Enum,Bounded)
data Suit = Hearts | Diamonds | Clubs | Spades deriving (Show,Eq,Ord,Enum,Bounded)


rankToCInt :: Rank -> CInt
rankToCInt Two = #{const StdDeck_Rank_2}
rankToCInt Three = #{const StdDeck_Rank_3}
rankToCInt Four = #{const StdDeck_Rank_4}
rankToCInt Five = #{const StdDeck_Rank_5}
rankToCInt Six = #{const StdDeck_Rank_6}
rankToCInt Seven = #{const StdDeck_Rank_7}
rankToCInt Eight = #{const StdDeck_Rank_8}
rankToCInt Nine = #{const StdDeck_Rank_9}
rankToCInt Ten = #{const StdDeck_Rank_TEN}
rankToCInt Jack = #{const StdDeck_Rank_JACK}
rankToCInt Queen = #{const StdDeck_Rank_QUEEN}
rankToCInt King = #{const StdDeck_Rank_KING}
rankToCInt Ace = #{const StdDeck_Rank_ACE}

suitToCInt :: Suit -> CInt
suitToCInt Hearts = #{const StdDeck_Suit_HEARTS}
suitToCInt Diamonds = #{const StdDeck_Suit_DIAMONDS}
suitToCInt Clubs = #{const StdDeck_Suit_CLUBS}
suitToCInt Spades = #{const StdDeck_Suit_SPADES}

{-
cintToSuit :: CInt -> Suit
cintToSuit #{const StdDeck_Suit_HEARTS} = Hearts
cintToSuit #{const StdDeck_Suit_DIAMONDS} = Diamonds
cintToSuit #{const StdDeck_Suit_CLUBS} = Clubs
cintToSuit #{const StdDeck_Suit_SPADES} = Spades
-}
