module WorldCup where 
    
--import Directories
import Prelude hiding (zip) 	
import Mod
import Data.List
import System.IO
import Data.Maybe
import Debug.Trace
import Control.Monad
import Control.Applicative

 --import Lists / Sets
import qualified Data.Map as M 
import qualified Data.Vec as V
import qualified Data.Set as S

--Variables / Objects
type Team=data					
type WorldCup=data
type GameResult=data

data WorldCup = WorldCup [Group]  deriving (Show)
data GroupName = A | B | C | D | E | F | G | H deriving (Show,Eq,Enum)				 
data Group = Group GroupName (Team,Team,Team,Team) deriving (Show)
data KnockoutStage = KnockoutStage [Team] deriving (Show)
data GameResult = Win | Lose | Draw deriving (Show,Eq)
data RankingModel = RankingModel {ratings :: [(Team,Ranking)]} deriving (Show)

--Teams & Pool Data		  
data Team = RUS | SDA | EGP | URG | 
            PRT | ESP | MOR | IRN | 
            FRA | AUS | PRU | DEN | 
            ARG | ICD | CRO | NIG | 
            BRA | SWZ | COS | SBA | 
            GER | MEX | SWD | SKR | 
            BEL | PAN | TUN | ENG |
            POL | SEN | COL | JPN | deriving (Show,Eq,Ord)		

-- Fifa World Rankings & ELO Rating Sport Index || Updated As of 13st June 2018

let fFRA = 1987, sFRA = 91.0 
    fCRT = 1853, sCRT = 78.5
    fURG = 1894, sURG = 80.0
    fPRT = 1970, sPRT = 87.0
    fESP = 2044, sESP = 90.0
    fARG = 1986, sARG = 88.5
    fPRU = 1915, sPRU = 83.0
    fBRA = 2142, sBRA = 90.5
    fMEX = 1850, sMEX = 82.5
    fBEL = 1939, sBEL = 86.0
    fGER = 2077, sGER = 91.5 
    fSER = 1777, sSER = 81.5
    fCOL = 1928, sCOL = 82.5
    fENG = 1948, sENG = 80.0

--Rule Engine

rules :: [(GroupName,Int)]
rules = [(A,1),(F,1),(B,1),(E,1),(C,1),(H,1),(D,1),(G,1),
         (B,2),(E,2),(A,2),(F,2),(D,2),(G,2),(C,2),(H,2)] 

scoreGame :: League -> ((Team,Team),GameResult) -> League
scoreGame r ((x,_),Win) = Map.insertWith (+) x 3 r
scoreGame r ((_,y),Lose) = Map.insertWith (+) y 3 r
scoreGame r ((x,y),Draw) = Map.insertWith (+) y 1 (Map.insertWith (+) x 1 r)
scoreGames :: League -> [((Team,Team),GameResult)] -> League
scoreGames = foldl scoreGame 

fixtures :: (Team,Team,Team,Team) -> [(Team,Team)]
fixtures (a,b,c,d) = [(a,b),(a,c),(a,d),(b,c),(b,d),(c,d)]
								
initialLeague :: (Team,Team,Team,Team) -> League
initialLeague (a,b,c,d) = Map.fromList [(a,0),(b,0),(c,0),(d,0)]

class Model a where
    play :: a -> Team -> Team -> GameResult
    winner :: a -> Team -> Team -> Team
	
instance Model RankingModel where
    play = play
    winner = winner

playGroup :: Model a => a -> Group -> League
playGroup model (Group _ t) = scoreGames (initialLeague t) 
(zip matches results) 
    where
      matches = fixtures t
      results = map (uncurry (play model)) matches :: [GameResult]    

Position :: [(GroupName,League)] -> (GroupName,Int) -> Team
Position s (n,x) | x == 1 = fst $ head sortedList
                       | x == 2 = fst $ head $ tail sortedList
                       | otherwise = error "Invalid Placement"
    where
      l = Map.toList $ fromJust (lookup n s)
      sortedList = sortBy (\(_,a) (_,b) -> compare b a) l

advanceToKnockOut :: Model a => WorldCup -> a -> KnockoutStage
advanceToKnockOut (WorldCup groups) model = KnockoutStage teams 
	where
    groupWinners = zip [A .. H] (map (playGroup model) groups) :: [(GroupName,League)]
    teams = map (lookupPosition groupWinners) rules

nextRound :: Model a => a -> KnockoutStage -> KnockoutStage
nextRound _ (KnockoutStage (x:[])) = KnockoutStage [x]
nextRound model (KnockoutStage teams) = KnockoutStage results 
	where
    len = length teams `div` 2
    matchUps = uncurry zip $ splitAt len teams
    results = map (uncurry (winner model)) matchUps

simulate :: Model a => WorldCup -> a -> Team
simulate wc model = head x 
	where
    knockOut = advanceToKnockOut wc model
    rounds = iterate (nextRound model) knockOut
    KnockoutStage x = rounds !! 4

--User Interface

Display::IO ()  				                         
Display=do
			putStrLn "		Daily Predictor				"
			putStrLn "									"
			putStr "		Press Any Key To Display	"
			key<-getLine
			compute
									
									
compute :: ([Int],Int,[[Int]]) -> Int
compute (ms,p,ps) = BinaryTree ms p
play :: RankingModel -> Team -> Team -> GameResult
play (RankingModel m) x y = case result of
             GT -> Win
             LT -> Lose
             EQ -> Draw
    where
      r1 = fromJust $ lookup x m
      r2 = fromJust $ lookup y m
      result = compare r1 r
	  
winner :: RankingModel -> Team -> Team -> Team
winner m x y = case result of
                  Win -> x
                  Lose -> y
                  Draw -> x 
    where
      result = play m x y
	where
    BinaryTree _ 0 = 0
    BinaryTree ms rounds = if set then 0 else 1 + lt + rt
      where
        (l,r) = splitAt (length ms `div` 2) ms
        lt = BinaryTree l (rounds-1)
        rt = BinaryTree r (rounds-1)
        set = all (>=rounds) ms
            if (l == 0 && r == 1)
            {
                --Sample for daily predictions
                prob_a = (((fARG)/(fARG+fSWZ))*100)
                prob_b = ((((fSWZ) / (fARG + fSWZ)) * 100))

                spi_a = (((sARG) / (sARG + sSWZ)) * 100)
                spi_b = (((sSWZ) / (sARG + sSWZ)) * 100)
                
                winner = (prob_a > prob_b) ? "Argentina" : "Switzerland"
                pHigh = (spi_a > spi_b) ? spi_a : spi_b 
                set = winner
                prob = pHigh                
            }
Result::String->Bool 				
Result s=set	