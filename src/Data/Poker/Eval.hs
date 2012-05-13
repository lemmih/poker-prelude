-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Poker.Parser
-- Copyright   :  PublicDomain
--
-- Maintainer  :  lemmih@gmail.com
-- Portability :  haskell2010
--
-- Primitive poker types such as Rank, Suit and Card.
--
module Data.Poker.Eval
    ( module Data.Poker.Definitions
    , module Data.Poker.Eval
    ) where

import Data.Poker.Definitions

import Foreign                   ( Ptr, alloca, allocaBytes, peek, FunPtr, withArray )
import Foreign.C
import System.IO.Unsafe
import Data.Word
import Data.Bits
import Data.IORef
import System.Random
import Text.Printf


newtype Card = Card CInt deriving (Eq,Ord)
instance Show Card where
    show = cardToString

instance Random Card where
    randomR (Card low,Card high) g
        = let (n, g') = randomR (fromIntegral low,fromIntegral high) g
          in (Card (fromIntegral (n :: Int)), g')
    random g = let (n, g') = randomR (0,51 :: Int) g
               in (Card (fromIntegral n), g')

newtype CardMask = CardMask { unmask :: StdDeck_CardMask } deriving (Eq,Ord)
instance Show CardMask where
    show mask = "maskFromString " ++ show (maskToString mask)


foreign import ccall "hs_StdDeck_MAKE_CARD" c_makeCard :: CInt -> CInt -> CInt
mkCard :: Rank -> Suit -> Card
mkCard rank suit
    = Card (c_makeCard (rankToCInt rank) (suitToCInt suit))

cardRank :: Card -> Rank
cardRank (Card idx) = toEnum (fromIntegral idx `mod` 13)

cardSuit :: Card -> Suit
cardSuit (Card idx) = toEnum (fromIntegral idx `div` 13)

foreign import ccall "StdDeck_stringToCard" c_stringToCard :: CString -> Ptr CInt -> IO CInt
stringToCard :: String -> Card
stringToCard txt
    = case stringToCard_ txt of
        Nothing   -> error $ "Data.Poker.stringToCard: No such card: " ++ txt
        Just card -> card

stringToCard_ :: String -> Maybe Card
stringToCard_ txt
    = unsafePerformIO $
      withCString txt $ \c_txt ->
      alloca $ \ptr ->
      do ret <- c_stringToCard c_txt ptr
         if ret == 2
            then do card <- peek ptr
                    return (Just (Card card))
            else    return Nothing

foreign import ccall "deck_std.h StdDeck_cardToString" c_cardToString :: CInt -> CString -> IO CInt
cardToString :: Card -> String
cardToString (Card idx)
    = unsafePerformIO $
      allocaBytes 3 $ \cstr ->
      do c_cardToString idx cstr
         peekCString cstr

foreign import ccall "hs_StdDeck_MASK" c_getMASK :: CInt -> StdDeck_CardMask

cardMask :: Card -> CardMask
cardMask (Card idx)
    = CardMask (c_getMASK idx)

foreign import ccall "hs_StdDeck_maskToString" c_maskToString :: StdDeck_CardMask -> CString -> IO CInt

maskFromString :: String -> CardMask
maskFromString = maskFromCards . map stringToCard . words

maskToString :: CardMask -> String
maskToString (CardMask m)
    = unsafePerformIO $
      allocaBytes 150 $ \cstr ->
      do c_maskToString m cstr
         peekCString cstr

foreign import ccall "hs_StdDeck_numCards" c_numCards :: StdDeck_CardMask -> CInt
numberOfCards :: CardMask -> Int
numberOfCards (CardMask m) = fromIntegral (c_numCards m)

maskOP :: (StdDeck_CardMask -> StdDeck_CardMask -> StdDeck_CardMask) -> CardMask -> CardMask -> CardMask
maskOP op (CardMask m1) (CardMask m2) = CardMask (op m1 m2)

maskUnOP :: (StdDeck_CardMask -> StdDeck_CardMask) -> CardMask -> CardMask
maskUnOP unop (CardMask m) = CardMask (unop m)

maskOr, maskAnd, maskXor :: CardMask -> CardMask -> CardMask
maskOr = maskOP (.|.)
maskAnd = maskOP (.&.)
maskXor = maskOP xor
maskComplement = maskUnOP complement

emptyMask :: CardMask
emptyMask = CardMask 0

maskFromCards :: [Card] -> CardMask
maskFromCards = foldl maskOr emptyMask . map cardMask

countRank :: CardMask -> Rank -> Int
countRank mask rank
    = numberOfCards (mask `maskAnd` rankMask)
    where rankMask = maskFromCards [ mkCard rank suit | suit <- [minBound .. maxBound]]

countSuit :: CardMask -> Suit -> Int
countSuit mask suit
    = numberOfCards (mask `maskAnd` suitMask)
    where suitMask = maskFromCards [ mkCard rank suit | rank <- [minBound .. maxBound]]

hasCard :: CardMask -> Card -> Bool
hasCard mask card
    = numberOfCards (mask `maskAnd` cardMask card) == 1

foreign import ccall "hs_StdDeck_StdRules_EVAL_N" c_eval_n :: StdDeck_CardMask -> CInt -> CInt
evalHand :: CardMask -> Int
evalHand (CardMask m)
    = fromIntegral (c_eval_n m (c_numCards m))



--void hs_DECK_ENUMERATE_5_CARDS_D(callback cb, hs_StdDeck_CardMask hs_dead)
foreign import ccall safe "hs_DECK_ENUMERATE_5_CARDS_D" c_enumerate_5_d :: FunPtr (StdDeck_CardMask -> IO ()) -> StdDeck_CardMask -> IO ()
foreign import ccall "wrapper" wrap :: (StdDeck_CardMask -> IO ()) -> IO (FunPtr (StdDeck_CardMask -> IO ()))

enumerateFiveCards :: (CardMask -> IO ()) -> CardMask -> IO ()
enumerateFiveCards fn (CardMask m)
    = do callback <- wrap (fn . CardMask)
         c_enumerate_5_d callback m

handStrenght :: CardMask -> CardMask -> IO ()
handStrenght hand against
    = do win  <- newIORef 0
         loss <- newIORef 0
         tie  <- newIORef 0
         enumerateFiveCards (\board -> case compare (evalHand (hand `maskOr` board)) (evalHand (against `maskOr` board)) of
                                         EQ -> modifyIORef tie succ
                                         LT -> modifyIORef loss succ
                                         GT -> modifyIORef win succ) (hand `maskOr` against)
         print =<< readIORef win
         print =<< readIORef loss
         print =<< readIORef tie

foreign import ccall "hs_enumExhaustive" c_enumExhaustive :: CInt -> Ptr StdDeck_CardMask -> StdDeck_CardMask
                                                          -> StdDeck_CardMask -> CInt -> CInt -> CInt -> Ptr EnumResult
                                                          -> IO CInt
enumExhaustive :: [CardMask] -> CardMask -> CardMask -> EnumResult
enumExhaustive pockets (CardMask board) (CardMask dead)
    = unsafePerformIO $
      withArray (map unmask pockets) $ \pocketsArray ->
      alloca $ \result ->
      do c_enumExhaustive 0 pocketsArray board dead (fromIntegral (length pockets)) (c_numCards board) 0 result
         peek result

printResult :: [CardMask] -> EnumResult -> IO ()
printResult pockets result
    = do printf      "Samples: %d\n" samples
         printf      "cards     win   %%win      lose  %%lose       tie   %%tie        EV\n"
         (flip mapM_) (zip pockets (resultPlayers result)) $ \(pocket,player) ->
           do let win  = playerWins player
                  lose = playerLosses player
                  tie  = playerTies player
                  win_p = p win
                  lose_p = p lose
                  tie_p = p tie
                  ev = playerEV player / fromIntegral samples
              printf "%5s %7d %6.2f %9d %6.2f %9d %6.2f %9.3f\n" (show pocket) win win_p lose lose_p tie tie_p ev
    where samples = resultSamples result
          p a = fromIntegral a / fromIntegral samples * 100 :: Double