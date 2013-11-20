module RTC8564 where

import System.Time
import Data.Bits
import Control.Applicative

getData :: (Eq a ,Num a) => a -> IO [Int]
getData addr = do
    t <- toUTCTime <$> getClockTime
    return $ case addr of
        0x00 -> undefined --control1
        0x01 -> undefined --control2
        0x02 -> bitShifter 7 $ ctSec t
        0x03 -> bitShifter 7 $ ctMin t
        0x04 -> bitShifter 6 $ ctHour t
        0x05 -> bitShifter 6 $ ctDay t
        0x06 -> bitShifter 3.fromWDay $ ctWDay t
        0x07 -> bitShifter 5.fromMonth $ ctMonth t --months/century
        0x08 -> bitShifter 8 $ ctYear t
        0x09 -> undefined --minute alarm
        0x0a -> undefined --hour alarm
        0x0b -> undefined --day alarm
        0x0c -> undefined --weekday alarm
        0x0d -> undefined --CLKOUT frequency
        0x0e -> undefined --timer control
        0x0f -> undefined --timer

bitShifter :: Int -> Int -> [Int]
bitShifter 0 v = [mod v 2]
bitShifter n v =  reverse $ shifter n v where
    shifter n v = bit:shifter (n-1) (div v 2)
    bit = if mod v 2 == 0 then 0 else 1
    
fromMonth January = 1
fromMonth February = 2
fromMonth March = 3
fromMonth April = 4
fromMonth May = 5
fromMonth June = 6
fromMonth July = 7
fromMonth August = 8
fromMonth September = 9
fromMonth October = 10
fromMonth November = 11
fromMonth December = 12

fromWDay Sunday = 0
fromWDay Monday = 1
fromWDay Tuesday = 2
fromWDay Wednesday = 3
fromWDay Thursday = 4
fromWDay Friday = 5
fromWDay Saturday = 6