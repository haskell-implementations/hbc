module Time(ClockTime, _mkClockTime,
	Month(..), Day(..),
	CalendarTime(..), 
	TimeDiff(..), timeDiffToDouble,
	getClockTime, addToClockTime, diffClockTime, 
	toCalendarTime, toUTCTime, 
	toClockTime,
	formatCalendarTime, calendarTimeToString,
	getTimeDouble
	) where

import _LibDialogue
import _LibIO__process
import Array
import Char
import Locale

data ClockTime = CT Double deriving (Ord, Eq)

unCT (CT d) = d

_mkClockTime d = CT d

instance Show ClockTime where
    showsType _ = showString "ClockTime"
    showsPrec d t = showString (formatCalendarTime defaultTimeLocale "%Y-%m-%d %H:%M:%S UTC" (toUTCTime t))

data Month = January | February | March | April | May | June | 
	July | August | September | October | November | December
	deriving (Eq, Ord, Enum, Bounded, Ix, Read, Show)

data Day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
	deriving (Eq, Ord, Enum, Bounded, Ix, Read, Show)

data CalendarTime = CalendarTime {
	ctYear :: Int,
	ctMon  :: Month,
	ctDay  :: Int,
	ctHour :: Int,
	ctMin  :: Int,
	ctSec  :: Int,
	ctPicosec :: Integer,
	ctWDay :: Day,
	ctYDay :: Int,
	ctTZName:: String,
	ctTZ   :: Int,
	ctIsDST:: Bool
	} deriving (Eq, Ord, Show, Read)

data TimeDiff = TimeDiff {
	tdYear :: Int,
	tdMon  :: Int,
	tdDay  :: Int,
	tdHour :: Int,
	tdMin  :: Int,
	tdSec  :: Int,
	tdPicosec :: Integer
	} deriving (Eq, Ord, Show, Read)

timeDiffToCalendarTime TimeDiff{ tdYear, tdMon, tdDay, tdHour, tdMin, tdSec, tdPicosec } =
	CalendarTime { ctYear = tdYear+1970, ctMon = toEnum tdMon, ctDay = tdDay, 
		       ctHour = tdHour, ctMin = tdMin, ctSec = tdSec, ctPicosec = tdPicosec}

calendarTimeToTimeDiff CalendarTime{ctYear, ctMon, ctDay, ctHour, ctMin, ctSec, ctPicosec} =
	TimeDiff { tdYear = ctYear-1970, tdMon = fromEnum ctMon, tdDay = ctDay, 
		   tdHour = ctHour, tdMin = ctMin, tdSec = ctSec, tdPicosec = ctPicosec }

timeDiffToDouble = unCT . calendarTimeToClockTime . timeDiffToCalendarTime

getClockTime :: IO ClockTime
getClockTime = 
	processRequestIODouble Nothing Nothing GetTime >>= \ d ->
	return (CT d)

getTimeDouble :: IO Double
getTimeDouble = 
	getClockTime >>= return . unCT

addToClockTime :: TimeDiff -> ClockTime -> ClockTime
addToClockTime td (CT d) = CT (d + unCT (calendarTimeToClockTime (timeDiffToCalendarTime td)))

diffClockTime :: ClockTime -> ClockTime -> TimeDiff
diffClockTime (CT d) (CT d') = calendarTimeToTimeDiff (clockTimeToCalendarTime' (CT (d - d')))

toCalendarTime :: ClockTime -> IO CalendarTime
toCalendarTime (CT d) =
	processRequestIOGetTimeZone Nothing Nothing (H_GetTimeZone d) >>= \ (b, s, i) ->
	return (clockTimeToCalendarTime (CT (d + fromInt i)) b s i)

toUTCTime :: ClockTime -> CalendarTime
toUTCTime ct = clockTimeToCalendarTime ct False "GMT" 0

toClockTime :: CalendarTime -> ClockTime
toClockTime t = calendarTimeToClockTime t

isleap :: Int -> Bool
isleap n = n `rem` 4 == 0			-- good enough for the UNIX time span

daysIn :: Int -> Int
daysIn n = if isleap n then 366 else 365

monthlen :: Array (Bool, Int) Int
monthlen = array ((False, 1), (True, 12)) 
	(zipWith3 (\ a b c -> ((a,b),c)) (repeat False) [1..] [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] ++
	 zipWith3 (\ a b c -> ((a,b),c)) (repeat True)  [1..] [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31])

clockTimeToCalendarTime' ct = clockTimeToCalendarTime ct undefined undefined undefined

clockTimeToCalendarTime :: ClockTime -> Bool -> String -> Int -> CalendarTime
clockTimeToCalendarTime (CT d) b s i = 
	let t = truncate d :: Int
	    (days, rem)  = t `quotRem` (60*60*24)
	    (hour, rem') = rem `quotRem` (60*60)
	    (min,  sec)  = rem' `quotRem` 60
	    wday         = (days+4) `mod` 7
	    (year, days')= until (\ (y, d) -> d < daysIn y) (\ (y, d) -> (y+1, d - daysIn y)) (1970, days)
	    (mon, day)   = until (\ (m, d) -> d < monthlen!(isleap year, m)) (\ (m, d) -> (m+1, d - monthlen!(isleap year, m))) (1, days')
	in  CalendarTime year (toEnum (mon-1)) (day+1) hour min sec (truncate (1.0e12*(d - fromInt t))) (toEnum wday) days' s i b

calendarTimeToClockTime :: CalendarTime -> ClockTime
calendarTimeToClockTime (CalendarTime year mont day hour min sec sdec _ _ s i b) =
	let mon = fromEnum mont
	    year'  = year - 1970
	    days   = year' * 365 + (year'+1) `div` 4 + 
		     sum [monthlen!(isleap year, m) | m <- [1..mon-1]] + day - 1
            secs   = ((days*24 + hour) * 60 + min) * 60 + sec
        in  CT (fromInt secs + fromInteger sdec * 1.0e-12)

calendarTimeToString :: CalendarTime -> String
calendarTimeToString ct = formatCalendarTime defaultTimeLocale "%c" ct

-----

show2, show2', show3 :: Int -> String
show2 x = [intToDigit (x `quot` 10), intToDigit (x `rem` 10)]

show2' x = if x < 10 then [ ' ', intToDigit x] else show2 x

show3 x = intToDigit (x `quot` 100) : show2 (x `rem` 100)

formatCalendarTime :: TimeLocale -> String -> CalendarTime -> String
formatCalendarTime l fmt ct@(CalendarTime year mon day hour min sec sdec wday yday tzname _ _) =
	doFmt fmt
  where doFmt ('%':c:cs) = decode c ++ doFmt cs
  	doFmt (c:cs) = c : doFmt cs
	doFmt "" = ""
	to12 h = let h' = h `mod` 12 in if h == 0 then 12 else h
	decode 'A' = fst (wdays l  !! fromEnum wday)
	decode 'a' = snd (wdays l  !! fromEnum wday)
	decode 'B' = fst (months l !! fromEnum mon)
	decode 'b' = snd (months l !! fromEnum mon)
	decode 'h' = snd (months l !! fromEnum mon)
	decode 'C' = show2 (year `quot` 100)
	decode 'c' = doFmt (dateTimeFmt l)
	decode 'D' = doFmt "%m/%d/%y"
	decode 'd' = show2 day
	decode 'e' = show2' day
	decode 'H' = show2 hour
	decode 'I' = show2 (to12 hour)
	decode 'j' = show3 yday
	decode 'k' = show2' hour
	decode 'l' = show2' (to12 hour)
	decode 'M' = show2 min
	decode 'm' = show2 (fromEnum mon+1)
	decode 'n' = "\n"
	decode 'p' = (if hour < 12 then fst else snd) (amPm l)
	decode 'R' = doFmt "%H:%M"
	decode 'r' = doFmt (time12Fmt l)
	decode 'T' = doFmt "%H:%M:%S"
	decode 't' = "\t"
	decode 'S' = show2 sec
	decode 's' = show (round (unCT (calendarTimeToClockTime ct)))
	decode 'U' = show2 ((yday + 7 - fromEnum wday) `div` 7)
	decode 'u' = show (let n = fromEnum wday in if n == 0 then 7 else n)
	decode 'V' = 
	    let (week, days) = (yday + 7 - if fromEnum wday > 0 then fromEnum wday - 1 else 6) `divMod` 7
            in  show2 (if days >= 4 then week+1 else if week == 0 then 53 else week)
	decode 'W' = show2 ((yday + 7 - if fromEnum wday > 0 then fromEnum wday - 1 else 6) `div` 7)
	decode 'w' = show (fromEnum wday)
	decode 'X' = doFmt (timeFmt l)
	decode 'x' = doFmt (dateFmt l)
	decode 'Y' = show year
	decode 'y' = show2 (year `rem` 100)
	decode 'Z' = tzname
	decode '%' = "%"
	decode c   = [c] --error ("formatCalendarTime: illegal formatting char "++[c])
