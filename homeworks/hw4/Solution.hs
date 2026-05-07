
{-# LANGUAGE InstanceSigs #-}

newtype Reader r a = Reader { runReader :: r -> a }

-- 1
instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader g) = Reader (f . g)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure x = Reader (const x)

  liftA2 :: (a -> b -> c) -> Reader r a -> Reader r b -> Reader r c
  liftA2 f (Reader g) (Reader h) = Reader (\r -> f (g r) (h r))

instance Monad (Reader r) where
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader g) >>= f = Reader (\r -> runReader (f (g r)) r)

-- 2
ask   :: Reader r r
ask = Reader id

asks  :: (r -> a) -> Reader r a
asks = Reader

local :: (r -> r) -> Reader r a -> Reader r a
local f (Reader g) = Reader (g . f)

-- 3
data BankConfig = BankConfig
  { interestRate   :: Double
  , transactionFee :: Int
  , minimumBalance :: Int
  } deriving (Show)

data Account = Account
  { accountId :: String
  , balance   :: Int
  } deriving (Show)


calculateInterest   :: Account -> Reader BankConfig Int
calculateInterest account = do 
    rate <- asks interestRate
    return $ floor (fromIntegral (balance account) * rate)

applyTransactionFee :: Account -> Reader BankConfig Account
applyTransactionFee account = do
    fee <- asks transactionFee
    return account { balance = balance account - fee }

checkMinimumBalance :: Account -> Reader BankConfig Bool
checkMinimumBalance account = do
    minBalance <- asks minimumBalance
    return (balance account >= minBalance)

processAccount      :: Account -> Reader BankConfig (Account, Int, Bool)
processAccount account = do
    interest <- calculateInterest account
    updatedAccount <- applyTransactionFee account
    meetsMinimum <- checkMinimumBalance account
    return (updatedAccount, interest, meetsMinimum)

main :: IO ()
main = do
    let cfg = BankConfig { interestRate = 0.05, transactionFee = 2, minimumBalance = 100 }
    let acc = Account { accountId = "A-001", balance = 1000 }
    print $ runReader (processAccount acc) cfg
