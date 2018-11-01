--@@ Unicode version of the Char module.
--@@ <br><b>NOTE</b> This module is incomplete.
--@@ Only plane 0 (16 bit) characters are supported.
module Char(
	isLatin1, isAscii, isControl,
        isDigit, isAlpha, isAlphanum, 
        isPrint, isSpace, 
        isHexDigit, isOctDigit,
        isUpper, isLower,
        toUpper, toLower,
        digitToInt, intToDigit,
        ord, chr,
        readLitChar, showLitChar,
	decodeUTF8, encodeUTF8, 
        decodeUnicode, encodeUnicode, 
        decodeEscape, encodeEscape
	) where
import _LibIO_all(_unsafePerformIO, IO)
import System
import Array
import _ByteVector

import Char__showLitChar
import Char__readLitChar

data GeneralCategory
    = None
 -- Normative
    | Mn -- Mark, Non-Spacing
    | Mc -- Mark, Spacing Combining
    | Me -- Mark, Enclosing

    | Nd -- Number, Decimal Digit
    | Nl -- Number, Letter
    | No -- Number, Other

    | Zs -- Separator, Space
    | Zl -- Separator, Line
    | Zp -- Separator, Paragraph

    | Cc -- Other, Control
    | Cf -- Other, Format
    | Cs -- Other, Surrogate
    | Co -- Other, Private Use
    | Cn -- Other, Not Assigned

  -- Informative
    | Lu -- Letter, Uppercase
    | Ll -- Letter, Lowercase
    | Lt -- Letter, Titlecase
    | Lm -- Letter, Modifier
    | Lo -- Letter, Other

    | Pc -- Punctuation, Connector
    | Pd -- Punctuation, Dash
    | Ps -- Punctuation, Open
    | Pe -- Punctuation, Close
    | Po -- Punctuation, Other

    | Sm -- Symbol, Math
    | Sc -- Symbol, Currency
    | Sk -- Symbol, Modifier
    | So -- Symbol, Other
    deriving (Eq, Ord, Enum)

data UniInfo
    = Lower (Maybe Char)
    | Upper (Maybe Char)
    | Digit (Maybe Int)
    | Other GeneralCategory

defaultDir = "/usr/local/lib/lmlc"
table = "UnicodeInfo"

readByteVector :: String -> IO _ByteVector
readByteVector name =
    readFile name >>= return . packBV

unicodeTable = _unsafePerformIO $ do
	dir <- getEnv "HBCDIR" `catch` (\ _ -> getEnv "LMLDIR" `catch` (\ _ -> return defaultDir))
        let file = dir ++ "/hlib1.3/" ++ table
--	putStrLn ("reading "++file)
        bv <- readByteVector file
--	putStrLn ("done\n")
	return bv

substrl bv s l = substrBV bv s (s+l)

decodeTable :: _ByteVector -> (Array Int Int, Array Int Int, _ByteVector)
decodeTable bv =
    let version = getNet32 (substrl bv 0 4)
        loffs = listArray (0, 63) [ getNet32 (substrl bv (i*4 + 4) 4) | i <- [ 0 .. 63 ] ]
        uoffs = listArray (0, 63) [ getNet32 (substrl bv (i*4 + 4) 4) | i <- [ 64 .. 127 ] ]
        info = substrl bv (128*4 + 4) 65536
    in  if version == 19970220 then
	    (loffs, uoffs, info)
        else
            error "UniChar: wrong table version"

(lowerOffs, upperOffs, info) = decodeTable unicodeTable

getInfo :: Char -> UniInfo
getInfo c =
    let b = fromEnum (indexBV info (fromEnum c))
        n = fromEnum c
        decDig 0 = Nothing
        decDig 1 = Just (n `mod` 16)
        decDig 2 = Just ((n-6) `mod` 16)
        addOffs o = if o == 0x80000000 then 
        		Nothing
                    else 
                        Just (toEnum (n + o))
    in  case b `div` 0x40 of
        0x0 -> 
            if b `div` 0x20 == 0x20 then
                Digit (decDig (b `mod` 0x20))
            else
                Other (toEnum b)
        0x1 -> error "getInfo"
        0x2 -> Lower (addOffs (upperOffs!(b `mod` 0x40)))
        0x3 -> Upper (addOffs (lowerOffs!(b `mod` 0x40)))

isLatin1 :: Char -> Bool
isLatin1 c              =  c >= '\0' && c < '\xff'

isAscii :: Char -> Bool
isAscii c	 	=  c >= '\0' && c < '\x80'

isControl :: Char -> Bool
isControl c		=  c < ' ' || c >= '\DEL' && c <= '\x9f'

isDigit :: Char -> Bool
isDigit c =
    if c >= '\0' && c < '\xff' then
        c >= '0' && c <= '9'
    else
        case getInfo c of
        Digit _ -> True
        _ -> False

digitToInt :: Char -> Int
digitToInt c =
    if c >= '\0' && c < '\xff' then
             if c >= '0' && c <= '9' then fromEnum c - fromEnum '0'
	else if c >= 'a' && c <= 'f' then fromEnum c - fromEnum 'a' + 10
	else if c >= 'A' && c <= 'F' then fromEnum c - fromEnum 'A' + 10
        else error "Char.digitToInt: not a digit"
    else
        case getInfo c of
        Digit (Just i) -> i
        _    -> error "Char.digitToInt: not a digit"

intToDigit :: Int -> Char
intToDigit i | i >= 0  && i <= 9  = toEnum (fromEnum '0' + i)
	     | i >= 10 && i <= 15 = toEnum (fromEnum 'a' + i - 10)
             | otherwise = error "Char.intToDigit: not a digit"

isAlphanum :: Char -> Bool
isAlphanum c = 
    if c >= '\0' && c < '\xff' then
        isAlpha c || isDigit c
    else
        case getInfo c of
        Lower _ -> True
        Upper _ -> True
        Digit _ -> True
        _ -> False

isAlpha :: Char -> Bool
isAlpha c = 
    if c >= '\0' && c < '\xff' then
        isUpper c || isLower c
    else
        case getInfo c of
        Lower _ -> True
        Upper _ -> True
        _ -> False

isUpper :: Char -> Bool
isUpper c = 
    if c >= '\0' && c < '\xff' then
        c >= 'A' && c <= 'Z' || 
	c >= 'À' && c <= 'Ö' || 
	c >= 'Ø' && c <= 'Þ'
    else
        case getInfo c of
        Upper _ -> True
        _ -> False

isLower :: Char -> Bool
isLower c = 
    if c >= '\0' && c < '\xff' then
        c >= 'a' && c <= 'z' || 
	c >= 'ß' && c <= 'ö' || 
	c >= 'ø' && c <= 'ÿ'
    else
        case getInfo c of
        Lower _ -> True
        _ -> False

toUpper :: Char -> Char
toUpper c =
    if c >= '\0' && c < '\xff' && c /= 'ÿ' then
        if isLower c && c /= 'ß' && c /= 'ÿ' then
            toEnum (fromEnum c - fromEnum 'a' + fromEnum 'A')
	else
            c
    else
        case getInfo c of
        Lower (Just c') -> c'
        _ -> c

toLower :: Char -> Char
toLower c =
    if c >= '\0' && c < '\xff' then
        if isUpper c then
	    toEnum (fromEnum c - fromEnum 'A' + fromEnum 'a')
        else
            c
    else
        case getInfo c of
        Upper (Just c') -> c'
        _ -> c

-- XXX
isSpace :: Char -> Bool
isSpace c		=  c == ' ' || c == '\t' || c == '\n' || 
			   c == '\r' || c == '\f' || c == '\v' || c == '\xa0'
isOctDigit c		=  c >= '0' && c <= '7'
isHexDigit c		=  isDigit c || c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f'
isPrint c		=  c >= ' ' && c <= '~' || c >= '\xa0' && c <= 'ÿ'


chr :: Int -> Char
chr i = toEnum i

ord :: Char -> Int
ord c = fromEnum c

-- Take a Unicode string and encode it as a string
-- with the UTF8 method.
decodeUTF8 :: String -> String
decodeUTF8 "" = ""
decodeUTF8 (c:cs) | c < '\x80' = c : decodeUTF8 cs
decodeUTF8 (c:c':cs) | '\xc0' <= c  && c  <= '\xdf' && 
		      '\x80' <= c' && c' <= '\xbf' =
	toEnum ((fromEnum c `mod` 0x20) * 0x40 + fromEnum c' `mod` 0x40) : decodeUTF8 cs
decodeUTF8 (c:c':c'':cs) | '\xe0' <= c   && c   <= '\xef' && 
		          '\x80' <= c'  && c'  <= '\xbf' &&
		          '\x80' <= c'' && c'' <= '\xbf' =
	toEnum ((fromEnum c `mod` 0x10 * 0x1000) + (fromEnum c' `mod` 0x40) * 0x40 + fromEnum c'' `mod` 0x40) : decodeUTF8 cs
decodeUTF8 _ = error "UniChar.decodeUTF8: bad data"

encodeUTF8 :: String -> String
encodeUTF8 "" = ""
encodeUTF8 (c:cs) =
	if c > '\u0000' && c < '\u0080' then
	    c : encodeUTF8 cs
	else if c < '\u0800' then
	    let i = fromEnum c
	    in  toEnum (0xc0 + i `div` 0x40) : 
	        toEnum (0x80 + i `mod` 0x40) : 
		encodeUTF8 cs
	else
	    let i = fromEnum c
	    in  toEnum (0xe0 + i `div` 0x1000) : 
	        toEnum (0x80 + (i `mod` 0x1000) `div` 0x40) : 
		toEnum (0x80 + i `mod` 0x40) : 
		encodeUTF8 cs

-- Take a Unicode string and encode it as a byte string
-- with 2 bytes per Char using "network" byte order.
encodeUnicode :: String -> String
encodeUnicode "" = ""
encodeUnicode (c:cs) =
	let ci = fromEnum c
	in  toEnum (ci `div` 256) : toEnum (ci `mod` 256) : encodeUnicode cs

decodeUnicode :: String -> String
decodeUnicode "" = ""
decodeUnicode (ch:cl:cs) = 
	toEnum (fromEnum ch * 256 + fromEnum cl) : decodeUnicode cs
decodeUnicode _ = error "UniChar.decodeUnicode: odd number of bytes"

-- Take a Unicode string and encode it as a string
-- with the \uXXXX (Java) convention for chars >= 256.
encodeEscape :: String -> String
encodeEscape "" = ""
encodeEscape (c:cs) =
    let hex 0 _ = ""
        hex n x = hex (n-1) (x `div` 16) ++ ["0123456789abcdef" !! (x `mod` 16)]
        i = fromEnum c
    in  if i > 255 || (c == '\\' && case cs of 'u':_ -> True; _ -> False) then
	    "\\u" ++ hex 4 i ++ encodeEscape cs
	else
	    c : encodeEscape cs

decodeEscape :: String -> String
decodeEscape "" = ""
decodeEscape ('\\':'u':c1:c2:c3:c4:cs) =
    toEnum (fromHex c1 * 0x1000 + fromHex c2 * 0x100 + fromHex c3 * 0x10 + fromHex c4) : decodeEscape cs
    where fromHex c = if isDigit c then fromEnum c - fromEnum '0' else if c >= 'a' then fromEnum c - fromEnum 'a' + 10 else fromEnum c - fromEnum 'A' + 10
decodeEscape (c:cs) = c : decodeEscape cs

--------------

getNet32 :: _ByteVector -> Int
getNet32 p = 
	fromEnum (indexBV p 0) * 0x1000000 + 
	fromEnum (indexBV p 1) * 0x10000 +
	fromEnum (indexBV p 2) * 0x100 +
	fromEnum (indexBV p 3)
