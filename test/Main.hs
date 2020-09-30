module Main where

import System.Exit
import Data.Time
import Data.Sched
import System.IO
import Control.Applicative
import Control.Monad

defaultTime = UTCTime { utctDay = ModifiedJulianDay { toModifiedJulianDay = 59120 }
                        , utctDayTime = timeOfDayToTime $
                            TimeOfDay { todHour = 19, todMin = 30, todSec = 15 }
                        }

addSeconds ds lt = addUTCTime (secondsToNominalDiffTime ds) lt


-- name
-- expected return
-- delta in seconds
-- local time to within
-- value in sched monad
-- local time to the sched
testWithin name expect ds ld ldd
    = TestCase { tcName = "within_" ++ name
               , tcRun = assertEq expect got
               }
        where got = runSched (within (secondsToNominalDiffTime ds) ld) ldd


-- name
-- expected return
-- local time to before
-- value in sched monad
-- local time to the sched
testSimpleFactory fn fnname = fnn
    where fnn name expect ld ldd = TestCase { tcName = fnname ++ name
                                            , tcRun = assertEq expect (runSched (fn ld) ldd)
                                              }

testBefore = testSimpleFactory before "before_"
testEndingAt = testSimpleFactory endingAt "endingAt"

testAfter = testSimpleFactory after "after_"
testStartingAt = testSimpleFactory startingAt "startingAt"

testSingle name expect sched time = TestCase { tcName = name
                                             , tcRun = assertEq expect (runSched sched time)
                                             }

testMonad name expect sched times
    = TestCase { tcName = "monad_"++name
               , tcRun = assertFold assertEq (map testMonad' times)
               }
        where
            testMonad' time = ((runSched expect time), (runSched sched time))

-- This is a helper function for the purposes of testing monad properties.
-- this behavior should be achieved with the do notation
withValue :: Sched () -> a -> Sched a
withValue s x = do
    s
    return x

main :: IO ()
main = do
    runTests [ testWithin "1"
                (End (succTime (addSeconds 10 defaultTime)) ())
                10 defaultTime
                defaultTime
             , testWithin "edge end"
                (End (succTime (addSeconds 10 defaultTime)) ())
                10 defaultTime
                (addSeconds 10 defaultTime)
             , testWithin "edge end 1"
                Never
                10 defaultTime
                (succTime (addSeconds 10 defaultTime))
             , testWithin "edge start"
                (End (succTime (addSeconds 10 defaultTime)) ())
                10 defaultTime
                (addSeconds (-10) defaultTime)
             , testWithin "edge start 1"
                (Start (addSeconds (-10) defaultTime))
                10 defaultTime
                (predTime (addSeconds (-10) defaultTime))
             , testWithin "2"
                Never
                10 defaultTime
                (addSeconds 11 defaultTime)
             , testWithin "3"
                (Start (addSeconds (-10) defaultTime))
                10 defaultTime
                (addSeconds (-11) defaultTime)

             , testBefore "1"
                (End defaultTime ())
                defaultTime
                (addSeconds (-10) defaultTime)
             , testBefore "2"
                Never
                defaultTime
                (addSeconds 10 defaultTime)
             , testBefore "edge_on"
                Never
                defaultTime
                defaultTime
             , testBefore "edge_after"
                Never
                defaultTime
                (succTime defaultTime)
             , testBefore "edge_before"
                (End defaultTime ())
                defaultTime
                (predTime defaultTime)

             , testEndingAt "1"
                (End (succTime defaultTime) ())
                defaultTime
                (addSeconds (-10) defaultTime)
             , testEndingAt "2"
                Never
                defaultTime
                (addSeconds 10 defaultTime)
             , testEndingAt "edge_on"
                (End (succTime defaultTime) ())
                defaultTime
                defaultTime
             , testEndingAt "edge_before"
                (End (succTime defaultTime) ())
                defaultTime
                (predTime defaultTime)
             , testEndingAt "edge_after"
                Never
                defaultTime
                (succTime defaultTime)

             , testAfter "1"
                (Start (succTime defaultTime))
                defaultTime
                (addSeconds (-10) defaultTime)
             , testAfter "2"
                (Whenever ())
                defaultTime
                (addSeconds 10 defaultTime)
             , testAfter "edge_on"
                (Start (succTime defaultTime))
                defaultTime
                defaultTime
             , testAfter "edge_before"
                (Start (succTime defaultTime))
                defaultTime
                (predTime defaultTime)
             , testAfter "edge_after"
                (Whenever ())
                defaultTime
                (succTime defaultTime)

             , testStartingAt "1"
                (Start defaultTime)
                defaultTime
                (addSeconds (-10) defaultTime)
             , testStartingAt "2"
                (Whenever ())
                defaultTime
                (addSeconds 10 defaultTime)
             , testStartingAt "edge_on"
                (Whenever ())
                defaultTime
                defaultTime
             , testStartingAt "edge_before"
                (Start defaultTime)
                defaultTime
                (predTime defaultTime)
             , testStartingAt "edge_after"
                (Whenever ())
                defaultTime
                (succTime defaultTime)

             , testSingle "monad_1"
                (End (addSeconds 10 defaultTime) ())
                (
                    (after (addSeconds (-10) defaultTime)) >>=
                    \x -> (before (addSeconds (10) defaultTime))
                )
                defaultTime
             , testSingle "monad_mutual_exclusive_bounds"
                Never
                (
                    (before (addSeconds (-10) defaultTime)) >>=
                    \x -> (after (addSeconds (10) defaultTime))
                )
                defaultTime
             , testSingle "monad_end_sooner"
                (End (addSeconds 10 defaultTime) ())
                (
                    (before (addSeconds (20) defaultTime)) >>=
                    \x -> (before (addSeconds (10) defaultTime))
                )
                defaultTime
             , testSingle "monad_start_first_bind"
                (Start (addSeconds 20 defaultTime))
                (
                    (startingAt (addSeconds (20) defaultTime)) >>=
                    \x -> (after (addSeconds (10) defaultTime))
                )
                defaultTime

             , testMonad "left_id"
                (
                    (\x -> withValue (before defaultTime) x) "q"
                )
                (
                    return "q" >>= (\x-> withValue (before defaultTime) x)
                )
                ([predTime, id, succTime] <*> [defaultTime])
             , testMonad "right_id"
                (
                    withValue (before defaultTime) "q"
                )
                (
                    (withValue (before defaultTime) "q") >>= return
                )
                ([predTime, id, succTime] <*> [defaultTime])
             , testMonad "assoc"
                (
                    ((withValue (after (addSeconds (-10) defaultTime)) "q") >>=
                    (\x -> withValue (before (addSeconds 10 defaultTime)) x)) >>=
                    (\x -> withValue (before (addSeconds 10 defaultTime)) (x++"a"))
                )
                (
                    (withValue (after (addSeconds (-10) defaultTime)) "q") >>=
                    (\x -> (withValue (before (addSeconds 10 defaultTime)) x) >>=
                        (\y -> withValue (before (addSeconds 10 defaultTime)) (y++"a")))
                )
                (map (\x -> addSeconds x defaultTime) [-11, -10, -9, 0, 9, 10, 11])
             , testMonad "assoc_1"
                (
                    ((withValue (after (addSeconds 5 defaultTime)) "q") >>=
                    (\x -> withValue (before (addSeconds 10 defaultTime)) x)) >>=
                    (\x -> withValue (before (addSeconds 10 defaultTime)) (x++"a"))
                )
                (
                    (withValue (after (addSeconds 5 defaultTime)) "q") >>=
                    (\x -> (withValue (before (addSeconds 10 defaultTime)) x) >>=
                        (\y -> withValue (before (addSeconds 10 defaultTime)) (y++"a")))
                )
                (map (\x -> addSeconds x defaultTime) [0, 4, 5, 6, 9, 10, 11])
             , testMonad "assoc_2"
                (
                    ((withValue (after (addSeconds 5 defaultTime)) "q") >>=
                    (\x -> withValue (before (addSeconds 1 defaultTime)) x)) >>=
                    (\x -> withValue (before (addSeconds 10 defaultTime)) (x++"a"))
                )
                (
                    (withValue (after (addSeconds 5 defaultTime)) "q") >>=
                    (\x -> (withValue (before (addSeconds 1 defaultTime)) x) >>=
                        (\y -> withValue (before (addSeconds 10 defaultTime)) (y++"a")))
                )
                (map (\x -> addSeconds x defaultTime) [-10, 0, 1, 2, 4, 5, 6, 9, 10, 11])
             , testMonad "assoc_3"
                (
                    ((withValue (after (addSeconds (-10) defaultTime)) "q") >>=
                    (\x -> withValue (before (addSeconds (-5) defaultTime)) x)) >>=
                    (\x -> withValue (before (addSeconds 10 defaultTime)) (x++"a"))
                )
                (
                    (withValue (after (addSeconds (-10) defaultTime)) "q") >>=
                    (\x -> (withValue (before (addSeconds (-5) defaultTime)) x) >>=
                        (\y -> withValue (before (addSeconds 10 defaultTime)) (y++"a")))
                )
                (map (\x -> addSeconds x defaultTime) [-11, -10, -9, -6, -5, -5, 0, 9, 10, 11])
             , testMonad "and_applicative"
                (
                    (withValue (before (addSeconds (-5) defaultTime)) reverse) <*>
                    (withValue (after (addSeconds (-10) defaultTime)) "qa")
                )
                (
                    (withValue (before (addSeconds (-5) defaultTime)) reverse) >>=
                    (\x1 -> (withValue (after (addSeconds (-10) defaultTime)) "qa") >>=
                        (\x2 -> return (x1 x2)))
                )
                (map (\x -> addSeconds x defaultTime) [-11, -10, -9, -6, -5, -5, 0])
             , testMonad "monad_plus_alternative"
                (
                    mzero >>=
                    (\_ -> after defaultTime)
                )
                (
                    mzero
                )
                (map (\x -> addSeconds x defaultTime) [-10, -1, 0, 1, 10])
             , testMonad "monad_plus_alternative_1"
                (
                    (after defaultTime) >> mzero :: Sched ()
                )
                (
                    mzero
                )
                (map (\x -> addSeconds x defaultTime) [-10, -1, 0, 1, 10])
             ]

data TestCase = TestCase { tcName :: String
                         , tcRun :: TestResult
                         }

data TestResult = Pass | Fail String | Skip String


assertFold :: (Show a, Eq a) => (a -> a -> TestResult) -> [(a, a)] -> TestResult
assertFold f xs = foldl assertFold' Pass xs
    where
        assertFold' prev (left, right) = case prev of Pass -> f left right
                                                      otherwise -> prev

assertEq:: (Show a, Eq a) => a -> a -> TestResult
assertEq l r = case l == r of True -> Pass
                              False -> Fail ("expected " ++ (show l) ++ " == " ++ (show r))

assertNeq :: (Show a, Eq a) => a -> a -> TestResult
assertNeq l r = case l == r of False -> Pass
                               True -> Fail ("expected " ++ (show l) ++ " != " ++ (show r))

assertOrd :: (Show a, Ord a) => Ordering -> a -> a -> TestResult
assertOrd o l r = case c of True -> Pass
                            False -> Fail ("expected " ++ (show l) ++ ostr ++ (show r))
    where c = (l `compare` r) == o
          ostr = " `" ++ (show o) ++ "` "

runTests :: [TestCase] -> IO ()
runTests tcs = mapM_ runTest tcs

runTest :: TestCase -> IO TestResult
runTest tc = do
    -- printErr $ tcStatus "RUNNING"
    case tcRes of Pass -> printErr (tcStatus "PASS")
                  Fail reason -> printErr ((tcStatus "FAIL") ++ ": " ++ reason)
                  Skip reason -> printErr ((tcStatus "SKIP") ++ ": " ++ reason)
    return tcRes
    where tcStatus s = s ++ " " ++ (tcName tc)
          tcRes = tcRun tc

printErr s = hPutStrLn stderr s
