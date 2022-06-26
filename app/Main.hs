{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import System.IO
import System.Random


main = do
  putStrLn "Hello, Haskell!"
  config <- initConfig
  amm <- initAmm config
  chan <- newChan
  forkIO $ threadReport (reportInterval config) chan
  let spawn (x:xs) = do
            forkIO $ threadUser x (avgTradeInterval config) chan
            when (xs /= []) $ spawn xs
  spawn $ users config
  runStateT (runReaderT (procTx chan) config) amm
  --return ()



type LpToken = Int 
type TokenA  = Int
type TokenB  = Int
type Percent = Float
type Id      = String

data Token
  = A TokenA
  | B TokenB deriving Show

data Event
  = ReportEvent
  | BuyEvent Percent
  | SellEvent Percent
  | DepositEvent Percent
  | WithdrawEvent Percent deriving Show



data User = User
  {
    name           :: Id,
    sellProbab     :: Float,
    buyProbab      :: Float,
    depositProbab  :: Float,
    withdrawProbab :: Float,
    delayFac       :: Float,
    tokenA         :: TokenA,
    tokenB         :: TokenB,
    lpToken        :: LpToken
  }   deriving (Show, Eq)

type Sec = Float

data Config = Config
  {
    users            :: [User],
    avgTradeInterval :: Sec,
    maxTxCnt         :: Int,
    reportInterval   :: Sec
  }   deriving Show

data Amm = Amm
  {
    usersLive  :: [User],
    poolLp     :: LpToken,
    lpTokenA   :: TokenA,
    lpTokenB   :: TokenB,
    lpK        :: Int
  }


initConfig :: IO Config
initConfig = do
  --Ask later how many traders, or from config file
  return $ Config
    {
      users            = [ User { name = "Bob"  , sellProbab = 0.05, buyProbab = 0.15, depositProbab = 0.005, withdrawProbab = 0.0001, delayFac = 1.5, tokenA = 10000, tokenB = 10000, lpToken = 0},
                           User { name = "Alice", sellProbab = 0.15, buyProbab = 0.05, depositProbab = 0.005, withdrawProbab = 0.0001, delayFac = 1.0, tokenA = 20000, tokenB = 5000, lpToken = 0},
                           User { name = "Ted"  , sellProbab = 0.05, buyProbab = 0.55, depositProbab = 0.005, withdrawProbab = 0.0001, delayFac = 3.5, tokenA = 200000, tokenB = 0, lpToken = 0}
                         ],
      avgTradeInterval = 1,
      maxTxCnt         = 10,
      reportInterval   = 5
    }
initAmm :: Config -> IO Amm
initAmm cfg = return $ Amm
  {
    usersLive  = users cfg, 
    poolLp     = 10 * 1 * (10^6) * 1 * (10^6),
    lpTokenA   = 1 * (10^6),
    lpTokenB   = 1 * (10^6),
    lpK        = 1 * (10^6) * 1 * (10^6) --can you use instead reviously defined lpTokenA and B?
  }


threadReport :: Sec -> Chan (User, Event) -> IO ()
threadReport iv chan = do
  let
    ghost = User { name = "Ghost"  , sellProbab = 0.00, buyProbab = 0.0, depositProbab = 0.000, withdrawProbab = 0.000, delayFac = 1.5, tokenA = 0, tokenB = 0, lpToken = 0}
  forever $ do
    writeChan chan (ghost, ReportEvent)
    threadDelay $ round $ iv * fromIntegral (10 ^ 6)

threadUser :: User -> Sec -> Chan (User, Event) -> IO ()
threadUser u secAvg chan = forever $ do
  --compute probab of trade interval delay
  pDelay <- (randomIO :: IO Float)
  let delayS | pDelay < 0.5 = ((pDelay + 0.5) * secAvg)
             | otherwise    = (1 + 5*(pDelay - 0.5)) * secAvg
  threadDelay $ round $ (delayFac u) * delayS * fromIntegral (10 ^ 6)
  --compute what trade action to do
  x <- (randomIO :: IO Float)
  y <- (randomIO :: IO Float)
  let normX = x * (sellProbab u + buyProbab u + depositProbab u + withdrawProbab u)
  let amt = y*0.05
  let action | normX < w 
               = WithdrawEvent amt
             | normX < d
               = DepositEvent amt
             | normX < b
               = BuyEvent amt
             | otherwise
               = SellEvent amt
             where
               w = withdrawProbab u
               d = w + depositProbab u
               b = d + buyProbab u
                
  --putStrLn $ "random: " ++ show (x) ++ " " ++ show (y)
  --putStrLn $ "User  : " ++ show (name u) ++ " Act: " ++ show (action)
  writeChan chan (u, action)




procTx :: Chan (User, Event) -> ReaderT Config (StateT Amm IO) ()
procTx chan = forever $ do
  (u, event) <- liftIO $ readChan chan
  if (name u) /= "Ghost"
    then do 
      liftIO $ putStrLn $ "User  : " ++ show (name u) ++ " Act: " ++ show (event)
      liftIO $ putStr $ "ProcTx: "
      case event of
        BuyEvent amt  -> do buyFromAmm (name u) amt; liftIO $ putStrLn $ "-------------------------> Bought!"; return ()
        SellEvent amt -> do sellToAmm (name u) amt; liftIO $ putStrLn $  "-------------------------> Sold!"; return ()
        ReportEvent   -> do reportAmmLedger; return ()
        _ -> return ()
  else do
    reportAmmLedger
    return ()


buyFromAmm :: (MonadState Amm m) => Id -> Float -> m ()
buyFromAmm id amt = do
  amm <- get;
  let 
    x  = [u | u <- (usersLive amm), (name u) == id]
    xs = [u | u <- (usersLive amm), (name u) /= id]
  case x of
    [x] -> do 
           let
             spendA  = round $ amt * fromIntegral (tokenA x)
             k       = lpK amm
             takeB   = (lpTokenB amm) - k `div` ((lpTokenA amm) + spendA)
             newAmmA = (lpTokenA amm) + spendA
             newAmmB = (lpTokenB amm) - takeB
             newX    = x { tokenA = tokenA x - spendA,
                           tokenB = tokenB x + takeB
                         }
           put ( amm { lpTokenA = newAmmA, lpTokenB = newAmmB,
                       usersLive = (newX : xs)
                     } )
           return ()
    _   -> return () --Will just return, but how do I trace for error in this context?
  return ()



sellToAmm :: (MonadState Amm m) => Id -> Float -> m ()
sellToAmm id amt = do
  amm <- get;
  let
    x  = [u | u <- (usersLive amm), (name u) == id]
    xs = [u | u <- (usersLive amm), (name u) /= id]
  case x of
    [x] -> do
           let
             spendB  = round $ amt * fromIntegral (tokenB x)
             k       = lpK amm
             takeA   = (lpTokenA amm) - k `div` ((lpTokenB amm) + spendB)
             newAmmB = (lpTokenB amm) + spendB
             newAmmA = (lpTokenA amm) - takeA
             newX    = x { tokenA = tokenA x + takeA,
                           tokenB = tokenB x - spendB
                         }
           put ( amm { lpTokenA = newAmmA, lpTokenB = newAmmB,
                       usersLive = (newX : xs)
                     } )
           return ()
    _   -> return () --Will just return, but how do I trace for error in this context?
  return ()


reportAmmLedger :: (MonadReader Config m, MonadState Amm m, MonadIO m) => m ()
reportAmmLedger = do
  config <- ask
  amm <- get
  let (sumA, sumB, sumLP) = foldl (\(accA, accB, accLP) x -> (accA + (tokenA x), accB + (tokenB x), accLP + (lpToken x))) (lpTokenA amm,  lpTokenB amm,  poolLp amm) (usersLive amm)
  liftIO $ putStrLn $ "/-----------------------------------------------------------------------------------------------\\"
  liftIO $ putStrLn $ "AMM   \t: Token A: " ++ show (lpTokenA amm) ++ ", Token B: " ++ show (lpTokenB amm) ++ ", LP Token: " ++ show (poolLp amm)
  let reportUser (x:xs) = do
                        liftIO $ putStrLn $ show (name x) ++ "\t: Token A: " ++ show (tokenA x) ++ ", Token B: " ++ show (tokenB x)  ++ ", LP Token: " ++ show (lpToken x)
                        when (xs /= []) $ reportUser xs
  reportUser $ usersLive amm
  liftIO $ putStrLn $ "Total \t: Token A: " ++ show (sumA) ++ ", Token B: " ++ show (sumB) ++ ", LP Token: " ++ show (sumLP)
  liftIO $ putStrLn $ "Price \t: " ++ show ((fromIntegral (lpTokenA amm)) / (fromIntegral (lpTokenB amm)))
  liftIO $ putStrLn $ "\\-----------------------------------------------------------------------------------------------/"
  return ()
