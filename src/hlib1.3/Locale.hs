module Locale(TimeLocale(..), defaultTimeLocale) where

data TimeLocale = TimeLocale {
	wdays :: [(String, String)],		-- full and abbreviated week days
	months :: [(String, String)],		-- full and abbreviated months
	amPm :: (String, String),		-- AM/PM symbols
	dateTimeFmt, dateFmt, timeFmt, time12Fmt :: String -- formatting strings
	} deriving (Eq, Ord, Show)

defaultTimeLocale :: TimeLocale 
defaultTimeLocale = TimeLocale { 
	wdays  = [("Sunday", "Sun"), ("Monday", "Mon"), ("Tuesday", "Tue"), ("Wednesday", "Wed"), ("Thursday", "Thu"), ("Friday", "Fri"), ("Saturday", "Sat")],
	months = [("January", "Jan"), ("February", "Feb"), ("March", "Mar"), ("April", "Apr"), ("May", "May"), ("June", "Jun"), ("July", "Jul"), ("August", "Aug"), ("September", "Sep"), ("October", "Oct"), ("November", "Nov"), ("December", "Dec")],
	amPm = ("AM", "PM"),
	dateTimeFmt = "%a %b %e %H:%M:%S %Z %Y",
	dateFmt = "%m/%d/%y",
	timeFmt = "%H:%M:%S",
	time12Fmt = "%I:%M:%S %p"
	}
