{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable #-}
module Data.Poker.State where

import Data.Acid
import Data.SafeCopy
import Data.Serialize
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad.Reader
import Foreign.C.Types
import Data.Typeable

import Data.Poker.Parser
import Data.Poker.Eval

data PokerState = PokerState { stateHistories :: Set.Set History } deriving (Typeable)

instance SafeCopy CardMask where
    putCopy mask = contain $ safePut (unmask mask)
    getCopy = contain $ fmap CardMask safeGet

$(deriveSafeCopy 1 'base ''Action)

instance SafeCopy CLLong where
    putCopy long = contain $ safePut (fromIntegral long :: Integer)
    getCopy = contain $ fmap (fromIntegral :: Integer -> CLLong) safeGet

$(deriveSafeCopy 1 'base ''History)
$(deriveSafeCopy 1 'base ''PokerState)

addHistory :: History -> Update PokerState ()
addHistory history
    = return () -- modify $ \(PokerState set) -> PokerState (Set.insert history set)

countHistories :: Query PokerState Int
countHistories
    = asks $ \(PokerState set) -> Set.size set

$(makeAcidic ''PokerState [ 'addHistory
                          , 'countHistories ])


openPokerState :: IO (AcidState PokerState)
openPokerState = openLocalState (PokerState Set.empty)

