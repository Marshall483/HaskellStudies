module Main where

data TBank = TBank 
   { name :: String 
   , balance :: Int
   , transfer :: Int
   , totalSum :: Int
   } deriving (Show)

getTotal :: TBank -> Int
getTotal ( TBank _ _ _ a ) = a
 
getTransf :: TBank -> Int
getTransf ( TBank _ _ g _ ) = g

isWithdrawOk :: TBank -> Int -> Bool
isWithdrawOk b amount = do
  let aGG = getTotal
  let bGG = getTransf
  print aGG
  let averageTransfer = div ax bx
    in balance b >= amount && (amount <= 1000 || transfer b > 10) && (amount <= 5 * averageTransfer)


-- && (amount <= 5 * averageTransfer)

addMoney :: TBank -> Int -> TBank
addMoney b amount = do
  if ( isWithdrawOk b amount )
    then b { balance = balance b + amount , transfer = transfer b + 1 , totalSum = totalSum b + amount }
    else b
--addMoney b amount = b { balance = balance b + amount , transfer = transfer b + 1 , totalSum = totalSum b + amount }
--addMoney b amount = b (\(n ,ba, t, tot) -> TBank {name =  name b, balance = balance b, transfer = transfer b + amount , totalSum = totalSum b + 1} )
    -- b {  }
    -- return b
withdraw :: TBank -> Int -> TBank
withdraw b amountm = do 
    if ( isWithdrawOk b amountm ) 
      then b { balance = balance b - amountm, transfer = transfer b + 1, totalSum = totalSum b + amountm }
      else b
    --TBank {name = name b, balance = balance b - amountm + a, transfer = transfer b + 1, totalSum = totalSum b + amountm}
    

testAlisa = TBank "Alisa" 0 0 0
testBob = TBank "Bob" 0 0 0

main :: IO ()
main = do
  let result =  addMoney testAlisa 100
  print result
  let ares =  withdraw result 100
  print ares
  let retur3 = addMoney testBob 100
  print ares
  print retur3
  let valueali = addMoney ares 50
  let valbob = withdraw retur3 50
  print valueali
  print valbob