import Data.Time
import Data.Sched

main = do
    tz <- getCurrentTimeZone
    c <- getCurrentTime

    let (day, tod) = localToUTCTimeOfDay tz midnight
    let tomorrowStart = UTCTime { utctDay = addDays (day+1) (utctDay c)
                                , utctDayTime = timeOfDayToTime tod
                                }
    let afterTomorrowStart = UTCTime { utctDay = addDays (day + 2) (utctDay c)
                                     , utctDayTime = timeOfDayToTime tod
                                     }

    let schedComb = do
        startingAt tomorrowStart
        before afterTomorrowStart
        return "it's tomorrow!"

    print $ schedMatchToZoned tz (runSched schedComb c)
    print $ schedMatchToZoned tz (runSched schedComb
        (addUTCTime (secondsToNominalDiffTime 60*60*24) c))
