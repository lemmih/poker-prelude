-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Poker.Parser
-- Copyright   :  PublicDomain
--
-- Maintainer  :  lemmih@gmail.com
-- Portability :  haskell2010
--
-- Parser for poker hands from hand-histories.com
--
module Data.Poker.Parser
       ( Token
       , PlayerName
       , Action(..)
       , History(..)
       , parseGames
       , mkHistory
       ) where

import Data.Poker.Eval

import Control.Applicative hiding ((<|>),many)
import Control.Monad
import Text.Parsec
import Text.Parsec.Text.Lazy
import Text.Parsec.Char
import Data.List
import Data.Char
import Data.Maybe

import System.Directory
import System.FilePath
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Read as T
import qualified Data.Text.Lazy.IO as T

{-
handDirectory = "/home/david/poker/lemmih_gmail_com_PTY_NLH25-USD_3-6plrs_x400k_fe9ae"

processHands
    = do files <- getDirectoryContents handDirectory
         dat <- sequence [ T.readFile (handDirectory </> file) | file <- files, file `notElem` [".", ".."]]
         print (take 2 $ drop 2 files)
         forM_ dat $ \inp ->
           do let hds = parseGames inp{-
              forM_ hds $ \tokens -> do mapM_ print tokens
                                        print (mkHistory tokens)-}
              print (mkHistory $ last hds)
         return ()
-}



--test = parseFromFile (many (try parseGame)) "pokergame"

--skipLine = many (noneOf "\n") >> newline
skipLine = do modInput_ $ T.dropWhile (/= '\n')
              newline

modInput fn
    = do inp <- getInput
         case fn inp of
           Right (val,rest) -> do setInput rest; return val
           Left msg         -> fail msg
modInput_ fn
    = modInput (\inp -> Right ((), fn inp))

digits :: Parser Int
digits = modInput $ T.decimal
--digits = fmap read (many1 digit)

cents :: Parser Integer
cents = do d <- modInput $ T.double 
           return (round $ d * 100)

{-cents = do str <- many1 (digit <|> oneOf ".")
            return (round $ read str * 100)
 <|> do dollars <- digits
          return (fromIntegral $ dollars * 100)-}

playerName :: Parser PlayerName
--playerName = many1 (satisfy (not . isSpace))
playerName = do inp <- getInput
                let (name, rest) = T.breakOn (T.pack " ") inp
                guard (not $ T.null name)
                setInput rest
                return name

str :: T.Text -> Parser ()
str s = try $ do char (T.head s)
                 modInput $ \inp -> case T.stripPrefix (T.tail s) inp of
                                      Nothing   -> Left ""
                                      Just rest -> Right ((),rest)

playerPrefixed :: T.Text -> Parser PlayerName
playerPrefixed suffix
  = try $
    do c <- anyToken
       modInput $ \inp -> let line:lines = T.lines inp in
                          case if T.null inp then [] else T.splitOn suffix line of
                            [before]     -> Left ""
                            before:after -> Right (T.cons c before, T.unlines (after ++ lines))
                            _ -> Left ""

data Token = DealerSeatTok Int
           | Seat Int PlayerName Integer -- Seat number nick stack
           | SBTok PlayerName Integer
           | BBTok PlayerName Integer
           | BBDeadTok PlayerName Integer
           | FoldsTok PlayerName
           | RaisesTok PlayerName Integer
           | BetsTok PlayerName Integer
           | CallsTok PlayerName Integer
           | ChecksTok PlayerName
           | CommunityTok CardMask
           | AllInTok PlayerName Integer
           | ShowsTok PlayerName CardMask
             deriving (Show)

parseGames inp
    = mapMaybe worker (T.splitOn (T.pack "\n\n") inp)
    where worker game = case parse parseGame "game" game of
                          Left err -> Nothing
                          Right toks -> Just toks
        
parseGame
    = many1 parseToken

parseToken
  = choice $
    [ dealingFlop
    , dealingTurn
    , dealingRiver
    , try dealerSeatTok
    , try seatTok
    , try $
      do choice $
           [ --foldsTok
             checksTok
           , raisesTok
           , betsTok
           , callsTok
           , allInTok
           , showsTok
           , sbTok
           , bbTok
           , bbDeadTok
           ]
    --, try (playerName >>= sbTok)
    --, try (playerName >>= bbTok)
    {-, try foldsTok, try checksTok
    , try raisesTok, try betsTok
    , try callsTok
    , try allInTok
    , try showsTok-}
    , try foldsTok
    , try $ skipLine *> parseToken
    ]

dealerSeatTok
  = str seatStr *> fmap DealerSeatTok digits <* str (T.pack " is the button\n")

seatStr = T.pack "Seat "
colonStr = T.pack ": "
dollarStr = T.pack " ( $"

seatTok
  = do n <- str seatStr *> digits <* str colonStr
       name <- playerPrefixed dollarStr
       stackCents <- cents <* skipLine
       return (Seat n name stackCents)

sbTokStr = T.pack " posts small blind [$"
sbTok
  = SBTok <$> playerPrefixed sbTokStr <*> cents <* skipLine

bbTok
  = BBTok <$> playerPrefixed (T.pack " posts big blind [$") <*> cents <* skipLine
bbDeadTok
  = BBDeadTok <$> playerPrefixed (T.pack " posts big blind + dead [$") <*> cents <* skipLine

card = stringToCard <$> count 2 anyChar
cards = fmap maskFromCards (card `sepBy` str (T.pack ", "))

dealingFlop
  = do str (T.pack "** Dealing Flop ** [ ")
       cards <- cards
       skipLine
       return $ CommunityTok cards

dealingTurn
  = str (T.pack "** Dealing Turn ** [ ") *> fmap CommunityTok cards <* skipLine

dealingRiver
  = str (T.pack "** Dealing River ** [ ") *> fmap CommunityTok cards <* skipLine

foldsTok
  = FoldsTok <$> playerPrefixed (T.pack " folds") <* skipLine

checksTok
  = ChecksTok <$> playerPrefixed (T.pack " checks") <* skipLine

raisesTok
  = RaisesTok <$> playerPrefixed (T.pack " raises [$") <*> cents <* skipLine

betsTok
  = BetsTok <$> playerPrefixed (T.pack " bets [$") <*> cents <* skipLine

callsTok
  = CallsTok <$> playerPrefixed (T.pack " calls [$") <*> cents <* skipLine

allInTok
  = AllInTok <$> playerPrefixed (T.pack " is all-In  [$") <*> cents <* skipLine

showsTok
  =  ShowsTok <$> playerPrefixed (T.pack " shows [ ") <*> cards <* skipLine
 <|> ShowsTok <$> playerPrefixed (T.pack " doesn't show [ ") <*> cards <* skipLine




type PlayerName = T.Text
data Action = Fold | Check | Bet Integer deriving (Show,Eq,Ord)
data History = History { historySB :: Integer
                       , historyBB :: Integer
                       , historyBBDead :: [(PlayerName, Integer)]
                       , historyPlayers :: [(PlayerName, Integer)]
                       , historyActions :: [[Action]]
                       , historyCommunity :: [CardMask] -- Community cards in the order they were shown.
                       , historyHands :: [(PlayerName,CardMask)]
                       } deriving (Show,Eq,Ord)

{-
data Token = DealerSeatTok Int
           | Seat Int PlayerName Integer -- Seat number nick stack
           | SBTok PlayerName
           | BBTok PlayerName
           | FoldsTok PlayerName
           | RaisesTok PlayerName Integer
           | CallsTok PlayerName Integer
           | DealingFlop [Card]
           | AllInTok PlayerName Integer
           | DealingTurn Card
           | DealingRiver Card
           | ShowsTok PlayerName [Card]
             deriving (Show)
-}
mkHistory :: [Token] -> History
mkHistory tokens
  = History { historySB = sb
            , historyBB = bb
            , historyBBDead = [ (player, deadBlind) | BBDeadTok player deadBlind <- tokens ]
            , historyPlayers = players
            , historyActions = mkActions tokens
            , historyCommunity = [ cards | CommunityTok cards <- tokens]
            , historyHands = [ (player, hand) | ShowsTok player hand <- tokens ]
            }
  where [sb] = take 1 ([ sb | SBTok _ sb <- tokens ] ++ [0])
        [bb] = take 1 ([ bb | BBTok _ bb <- tokens ] ++ [0])
        DealerSeatTok dealer = head tokens
        players = [ (player, stack) | i <- rotate dealer [1..9] -- Make the list with the dealer first.
                             , Seat n player stack <- tokens
                             , n==i
                             , player `elem` activePlayers tokens ]

mkActions tokens
    = worker [] tokens
    where worker acc [] = [reverse acc]
          worker acc (token:tokens)
              = case token of
                  FoldsTok{}         -> worker (Fold:acc) tokens
                  ChecksTok{}        -> worker (Check:acc) tokens
                  RaisesTok _ amount -> worker (Bet amount:acc) tokens
                  BetsTok _ amount   -> worker (Bet amount:acc) tokens
                  CallsTok _ amount  -> worker (Bet amount:acc) tokens
                  AllInTok _ amount  -> worker (Bet amount:acc) tokens
                  CommunityTok{}     -> reverse acc : worker [] tokens
                  _                  -> worker acc tokens

rotate n lst = take (length lst) . drop n . cycle $ lst

activePlayers :: [Token] -> [PlayerName]
activePlayers = nub . mapMaybe worker
  where worker (FoldsTok player) = Just player
        worker (ChecksTok player) = Just player
        worker (RaisesTok player _) = Just player
        worker (CallsTok player _) = Just player
        worker (SBTok player _) = Just player
        worker (BBTok player _) = Just player
        worker (BBDeadTok player _) = Just player
        worker _ = Nothing
