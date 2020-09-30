module Data.Sched where

import Control.Applicative
import Control.Monad
import Data.Time

newtype Sched a = Sched { unSched :: UTCTime -> SchedMatch a }


data ZonedSchedMatch a = ZonedEnd ZonedTime a
    | ZonedStart ZonedTime
    | ZonedWhenever a
    | ZonedNever
    deriving (Show)

schedMatchToZoned :: TimeZone -> SchedMatch a -> ZonedSchedMatch a
schedMatchToZoned tz s = case s of
    End utct x -> ZonedEnd (utcToZonedTime tz utct) x
    Start utct -> ZonedStart (utcToZonedTime tz utct)
    Whenever x -> ZonedWhenever x
    Never -> ZonedNever



-- End is not inclusive
-- Start is inclusive
data SchedMatch a = End UTCTime a
    | Start UTCTime
    | Whenever a
    | Never
    deriving (Eq, Show)

reSched :: (UTCTime -> SchedMatch a) -> Sched a
reSched s = Sched { unSched = s }


runSched :: Sched a -> UTCTime -> SchedMatch a
runSched s t = (unSched s) t

runSchedNow :: Sched a -> IO (SchedMatch a)
runSchedNow s = do
    c <- getCurrentTime
    return (runSched s c)

instance Functor Sched where
    fmap f sa = reSched $ \lt -> fmap' f sa lt
        where
            fmap' f sa lt =  case (runSched sa lt) of
                End ltt x -> End ltt (f x)
                Whenever x -> Whenever (f x)
                Start lt -> Start lt
                Never -> Never

instance Applicative Sched where
    pure x = reSched $ \lt -> Whenever x

    (<*>) sf sa = reSched $ \lt -> app' sf sa lt
        where
            app' sf sa lt = case (runSched sf lt) of
                End ltt f -> app'' f sa lt ltt
                Whenever f -> runSched (fmap f sa) lt
                Start ltt -> Start ltt
                Never -> Never
            app'' f sa lt ltt =  case (runSched sa lt) of
                End lttt x -> End (min ltt lttt) (f x)
                Whenever x -> End ltt (f x)
                Start lttt -> if lttt > ltt then Never else Start lttt
                Never -> Never

instance Alternative Sched where
    empty = reSched $ \_ -> Never

    (<|>) fa1 fa2 = reSched $ \utct -> alt' utct
        where
            alt'' utct utctt =
                case runSched fa2 utct of Start utcttt -> Start (min utctt utcttt)
                                          otherwise -> runSched fa2 utct
            alt' utct =
                case runSched fa1 utct of Never -> runSched fa2 utct
                                          Start utctt -> alt'' utct utctt
                                          otherwise -> runSched fa1 utct


instance Monad Sched where
    return = pure -- if this is changed, add a test case

    (>>=) sa fasb = reSched $  \lt -> flatMap' lt
        where
            flatMap' lt = case (runSched sa lt) of
                End ltt a -> flatMap'' a lt ltt
                Whenever a -> runSched (fasb a) lt
                Start ltt -> Start ltt
                Never -> Never
            flatMap'' a lt ltt = case (runSched (fasb a) lt) of
                End lttt b -> End (min ltt lttt) b
                Whenever b -> End ltt b
                Start lttt -> if lttt > ltt then Never else Start lttt
                Never -> Never

    (>>) sa sb = reSched $ \utct -> seqMap' utct
        where
            seqMap' utct = case runSched sa utct of
                End utctt _ -> seqMap'' utct utctt
                Whenever _ -> runSched sb utct
                Start utctt -> seqMap''' utct utctt
                Never -> Never
            seqMap'' utct utctt = case runSched sb utct of
                End utcttt x -> End (min utctt utcttt) x
                Whenever x -> End utctt x
                Start utcttt -> Start utcttt
                Never -> Never
            seqMap''' utct utctt = case runSched sb utct of
                End utcttt _ -> if utcttt < utctt then Never else Start utctt
                Whenever _ -> Start utctt
                Start utcttt -> Start (max utctt utcttt)
                Never -> Never

instance MonadPlus Sched where
    mzero = empty
    mplus = (<|>)


after :: UTCTime -> Sched ()
after lt = reSched $ \ltt -> after' lt ltt
    where
        after' lt ltt= case ltt > lt of
            True -> Whenever ()
            False -> Start (succTime lt)

startingAt :: UTCTime -> Sched ()
startingAt lt = reSched $ \ltt -> startingAt' lt ltt
    where
        startingAt' lt ltt = case ltt >= lt of
            True -> Whenever ()
            False -> Start lt

before:: UTCTime -> Sched ()
before utct = reSched $ \utctt -> before' utctt
    where
        before' utctt = case utctt < utct of
            True -> End utct ()
            False -> Never

endingAt :: UTCTime -> Sched ()
endingAt utct = reSched $ \utctt -> endingAt' utctt
    where
        endingAt' utctt = case utctt <= utct of
            True -> End (succTime utct ) ()
            False -> Never


within :: NominalDiffTime -> UTCTime -> Sched ()
within d lt = do
    startingAt (addUTCTime (-d') lt)
    endingAt (addUTCTime d' lt)
    where
        d' = abs d

onDayOf :: ZonedTime -> Sched ()
onDayOf zt = do
    startingAt todayStart
    before tomorrowStart
    where
        lt = zonedTimeToLocalTime zt
        intoUTC lt = zonedTimeToUTC $
            ZonedTime { zonedTimeToLocalTime = lt
                      , zonedTimeZone = (zonedTimeZone zt)
                      }
        todayStart' = LocalTime { localDay = localDay lt
                                , localTimeOfDay  = midnight
                                }
        tomorrowStart' = LocalTime { localDay = addDays 1 (localDay lt)
                                   , localTimeOfDay = midnight
                                   }
        todayStart = intoUTC todayStart'
        tomorrowStart = intoUTC tomorrowStart'

onDayOfWeek :: TimeZone -> DayOfWeek -> Sched ()
onDayOfWeek tz dow = reSched $ \utct -> onDayOfWeek' utct
    where
        intoLocal utct = zonedTimeToLocalTime $ utcToZonedTime tz utct
        nextDayOfWeek' utct = toInteger $
            (length [dayOfWeek (localDay (intoLocal utct)).. dow]) - 1
        nextDayOfWeek utct =
            addDaysZonedTime (nextDayOfWeek' utct) (utcToZonedTime tz utct)
        onDayOfWeek' utct = runSched (onDayOf (nextDayOfWeek utct)) utct

addDaysZonedTime :: Integer -> ZonedTime -> ZonedTime
addDaysZonedTime n zt = ZonedTime { zonedTimeToLocalTime = addedLocalTime
                                  , zonedTimeZone = zonedTimeZone zt
                                  }
    where
        localTime = zonedTimeToLocalTime zt
        addedLocalTime = LocalTime { localDay = addDays n (localDay localTime)
                                   , localTimeOfDay = localTimeOfDay localTime
                                   }

onHourOf :: ZonedTime -> Sched ()
onHourOf zt = do
    startingAt thisHour
    before nextHour
    where
        utct = zonedTimeToUTC zt
        lt = zonedTimeToLocalTime zt
        ltd = localDay lt
        ltod = localTimeOfDay lt
        ltod'  = LocalTime { localDay = ltd
                           , localTimeOfDay =
                                TimeOfDay { todHour = todHour ltod
                                          , todMin = 0
                                          , todSec = 0
                                          }
                           }
        thisHour = zonedTimeToUTC $
            ZonedTime { zonedTimeToLocalTime = ltod'
                      , zonedTimeZone  = zonedTimeZone zt
                      }
        nextHour = addUTCTime (secondsToNominalDiffTime 60*60) thisHour

onHour :: TimeZone -> TimeOfDay -> Sched ()
onHour tz tod = reSched $ \utct -> onHour' utct
    where
        intoLocal utct = zonedTimeToLocalTime $ utcToZonedTime tz utct
        intoLocalDay utct = localDay $ intoLocal utct
        intoLocalTod  utct = localTimeOfDay $ intoLocal utct
        tod' = TimeOfDay { todHour = todHour tod
                         , todMin = 0
                         , todSec = 0
                         }
        todaysTod utct =
            ZonedTime { zonedTimeToLocalTime =
                            LocalTime { localDay = intoLocalDay utct
                                      , localTimeOfDay = tod'
                                      }
                      , zonedTimeZone = tz
                      }
        tomorrowsTod utct =
            ZonedTime { zonedTimeToLocalTime =
                            LocalTime { localDay = addDays 1 (intoLocalDay utct)
                                      , localTimeOfDay = tod'
                                      }
                      , zonedTimeZone = tz
                      }
        nextHour utct = if intoLocalTod utct < tod' then todaysTod utct else tomorrowsTod utct
        onHour' utct = runSched (onHourOf (nextHour utct)) utct


succTime :: UTCTime -> UTCTime
succTime utct = (addUTCTime . succ) 0 utct

predTime :: UTCTime -> UTCTime
predTime utct = (addUTCTime . pred) 0 utct

