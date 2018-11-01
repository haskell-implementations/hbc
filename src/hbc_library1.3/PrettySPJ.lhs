*********************************************************************************
*										*
*	John Hughes's and Simon Peyton Jones's Pretty Printer Combinators	*
*										*
*		based on "The Design of a Pretty-printing Library"		*
*		in Advanced Functional Programming, 				*
*		Johan Jeuring and Erik Meijer (eds), LNCS 925 			*
*										*
*		Heavily modified by Simon Peyton Jones, Dec 96			*
*										*
*********************************************************************************


Relative to John's original paper, there are the following new features:

1.  There's an empty document, "empty".  It's a unit for just about
    everything, except that it's not a left unit for <> (see the 
    algebra below).  It is Really Useful in practice.

2.  There is a paragraph-fill combinator, fsep, that's much like sep,
    only it keeps fitting things on one line until itc can't fit any more.

3.  Some random useful extra combinators are provided.  
	<#> puts its arguments beside each other with a space between them,
		unless either argument is empty in which case it returns the other

	hcat is a list version of <>
	hsep is a list version of <#>
	vcat is a list version of $$

	cat  is behaves like sep,  but it uses <#> for horizontal conposition
	fcat is behaves like fsep, but it uses <#> for horizontal conposition

	These new ones do the obvious things:
		char, semi, comma, colon, space,
		parens, brackets, braces, 
		quotes, doubleQuotes
	
4.	The "above" combinator, $$, now overlaps its two arguments if the
	last line of the top argument stops before the first line of the second begins.
	For example:  text "hi" $$ nest 5 "there"
	lays out as
			hi   there
	rather than
			hi
			     there

	I'm not certain this is the right behaviour, but it's sometimes just
	what you want.  There ought to be an easy way of recovering the old
	behaviour, but I havn't worked out what it is yet.

5.	Several different renderers are provided:
		* a standard one
		* one that uses cut-marks to avoid deeply-nested documents 
			simply piling up in the right-hand margin
		* one that ignores indentation (fewer chars output; good for machines)
		* one that ignores indentation and newlines (ditto, only more so)

6.	Numerous implementation tidy-ups
	Use of unboxed data types to speed up the implementation



\begin{code}
#if !defined(TEST)
module PrettySPJ (
	Doc, 		-- Abstract
	Mode(..), 

	p_show, pp_show, 	-- Temporary

	empty,

	text, char, 
	parens, brackets, braces, quotes, doubleQuotes,
	semi, comma, colon, space,

	(<>), hcat, (<+>), (<#>), hsep, ($$), vcat, sep, cat, fsep, fcat, nest,
	
#if defined(__HASKELL_1_3__)
	renderStyle,
#endif
	render, fullRender
  ) where
#endif

infixl 6 <> 
infixl 6 <+>, <#>
infixl 5 $$
\end{code}



*********************************************************
*							*
\subsection{CPP magic so that we can compile with both GHC and Hugs}
*							*
*********************************************************

The library uses unboxed types to get a bit more speed, but these CPP macros
allow you to use either GHC or Hugs.  To get GHC, just set the CPP variable
	__GLASGOW_HASKELL__

\begin{code}
#if defined(TEST)
main = putStrLn (render (text "hej"))
#endif


#if defined(__GLASGOW_HASKELL__)

-- Glasgow Haskell

#define INT	Int#
#define DINT	INT
#define IBOX(x)	(I# x)
#define MINUS	-#
#define PLUS	+#
#define GR	>#
#define GREQ	>=#
#define LT	<#
#define DIV	`quotInt#`
#define ILIT(x) x#

-- Using GHC 0.29 at the moment, so Haskell 1.2
#define SHOW	Text
#define MAXINT	maxInt

#else

-- Standard Haskell

#define INT	Int
#if defined(__HASKELL_1_3__)
#define DINT	!Int
#else
#define DINT	INT
#endif
#define IBOX(x)	x
#define MINUS	-
#define PLUS	+
#define GR	>
#define GREQ	>=
#define LT	<
#define DIV	`quot`
#define ILIT(x) x

#define SHOW	Show
#define MAXINT	maxBound

#endif

#define ASSERT(x)
\end{code}


*********************************************************
*							*
\subsection{The interface}
*							*
*********************************************************

The primitive @Doc@ values

\begin{code}
empty  			  :: Doc
text			  :: String -> Doc 
char  			  :: Char -> Doc
semi, comma, colon, space :: Doc
parens, brackets, braces  :: Doc -> Doc 
quotes, doubleQuotes	  :: Doc -> Doc
\end{code}

Combining @Doc@ values

\begin{code}
(<>)   :: Doc -> Doc -> Doc	-- Beside
hcat   :: [Doc] -> Doc		-- List version of <>
(<+>)  :: Doc -> Doc -> Doc	-- Beside, separated by space
hsep   :: [Doc] -> Doc		-- List version of <+>

($$)   :: Doc -> Doc -> Doc 	-- Above; if there is no
				-- overlap it "dovetails" the two
vcat   :: [Doc] -> Doc		-- List version of $$

cat    :: [Doc] -> Doc		-- Either hcat or vcat
sep    :: [Doc] -> Doc		-- Either hsep or vcat
fcat   :: [Doc] -> Doc		-- ``Paragraph fill'' version of cat
fsep   :: [Doc] -> Doc		-- ``Paragraph fill'' version of sep

nest   :: Int -> Doc -> Doc	-- Nested
\end{code}

Displaying @Doc@ values. 

\begin{code}
instance SHOW Doc where
  showsPrec prec doc cont = showDoc doc cont

render     :: Doc -> String		-- Uses default style
fullRender :: Mode
	   -> Int			-- Line length
	   -> Float			-- Ribbons per line
	   -> Doc
	   -> String			-- Append this to the end
	   -> String

#if defined(__HASKELL_1_3__)
renderStyle  :: Style -> Doc -> String
data Style = Style { lineLength     :: Int,	-- In chars
		     ribbonsPerLine :: Float,	-- Ratio of ribbon length to line length
		     mode :: Mode
	     }
style :: Style		-- The default style
style = Style { lineLength = 100, ribbonsPerLine = 2.5, mode = PageMode }
#endif

data Mode = PageMode 		-- Normal 
	  | ZigZagMode		-- With zig-zag cuts
	  | LeftMode		-- No indentation
	  | OneLineMode		-- All on one line

\end{code}


*********************************************************
*							*
\subsection{The @Doc@ calculus}
*							*
*********************************************************

The @Doc@ combinators satisfy the following laws:
\begin{verbatim}
<1>	(x $$ y) $$ z	= x $$ (y $$ z)
<2>	empty $$ x	= x
<3>	(text s <> x) $$ y = text s <> ((text "" <> x)) $$ 
					 nest (-length s) y)

<4>	(x <> y) <> z	= x <> (y <> z)
<5>	(x $$ y) <> z	= x $$ (y <> z)
<6>	x <> empty	= x
<7>	x <> nest k y	= x <> y
<8>	empty <> text s = text s
<9>	empty <> (text s $$ x) = text s $$ x

<10>	nest k (x <> y)		= nest k z <> y
<11>	nest k (x $$ y)		= nest k x $$ nest k y
<12>	nest k (nest k' x)	= nest (k+k') x
<13>	nest 0 x		= x

<14>	text s <> text t	= text (s++t)
<15>	x $$ empty 		= x
<16>	nest k empty		= empty

<17> 	sep (ps++[empty]++qs)   = sep (ps ++ qs)
<18,19>	...ditto hsep, hcat, vcat, fill...
<20>	nest k (sep ps) = sep (map (nest k) ps)
<21,22>	...ditto hsep, hcat, vcat, fill...
\end{verbatim}

Notice that it is {\em not} the case that:
\begin{verbatim}
	empty <> x = x		NO!
\end{verbatim}
The latter would allow us to prove this nonsense:
\begin{verbatim}
	nest k x = empty <> nest k x	NO!
	         = empty <> x
		  = x
\end{verbatim}

You might think that the following verion of <3> would
be neater:
\begin{verbatim}
<3 NO>	(text s <> x) $$ y = text s <> ((empty <> x)) $$ 
					 nest (-length s) y)
\end{verbatim}
But it doesn't work, for if x=empty, we would have
\begin{verbatim}
	text s $$ y = text s <> (empty $$ nest (-length s) y)
		    = text s <> nest (-length s) y
\end{verbatim}



*********************************************************
*							*
\subsection{Simple derived definitions}
*							*
*********************************************************

\begin{code}
char c = text [c]
semi  = char ';'
colon = char ':'
comma = char ','
space = char ' '

quotes p	= char '`' <> p <> char '\''
doubleQuotes p	= char '"' <> p <> char '"'
parens p	= char '(' <> p <> char ')'
brackets p	= char '[' <> p <> char ']'
braces p	= char '{' <> p <> char '}'


hcat = foldr (<>)  empty
hsep = foldr (<#>) empty
vcat = foldr ($$)  empty

p1 <+> p2 = p1 <> space <> p2
\end{code}


*********************************************************
*							*
\subsection{The @Doc@ data type}
*							*
*********************************************************

A @Doc@ represents a {\em set} of layouts.  A @Doc@ with
no occurrences of @Union@ or @NoDoc@ represents just one layout.
\begin{code}
data Doc
 = Empty				-- empty
 | NilAbove Doc			-- text "" $$ x
 | TextBeside [Char] DINT Doc	-- text s <> x	
 | Nest DINT Doc			-- nest k x
 | Union Doc Doc			-- ul `union` ur
 | NoDoc				-- The empty set of pretties
\end{code}

Here is the data type more precisely expressed.  
We don't actually do it this
way because it adds too many extra coercion constructors.

\begin{verbatim}
data Doc	= Empty
		| Nest DINT Doc
		| DocSet DocSet

data DocSet	= NoDoc
		| Union DocSet DocSet
		| DocTerm DocTerm

data DocTerm = NilAbove Doc
		| TextBeside CSeq DINT Doc
\end{verbatim}

Here are the invariants:
\begin{itemize}
\item The arguments of @Union@ are either @TextBeside@, or @NilAbove@ (this is really the only invariant
encoded by the more complex data type).

\item The argument of @NilAbove@ is never an empty @Doc@. Therefore
a @NilAbove@ occupies at least two lines.

\item The layouts of the two arguments of @Union@ both flatten to the same string.

\item The first line of every layout in the left argument of @Union@
is longer than the first
line of any layout in the right argument.  (1) ensures that the left
argument has a first line.
In view of (3), this invariant means that the right argument
must have at least two
lines.

\item If the first argument of a union is the empty set (@NoDoc@),
then the @NoDoc@ appears in the first line.

\item The right argument of a union cannot be the empty set.
\end{itemize}

Notice the difference between
	* NoDoc (no documents)
	* Empty (one empty document; no height and no width)
	* text "" (a document containing the empty string;
		   one line high, but has no width)

Debugging printing code
~~~~~~~~~~~~~~~~~~~~~~~
\begin{code}
pp_show :: Doc -> String
pp_show Empty = "Emp"
pp_show (NilAbove p) = "(NA " ++ (pp_show p) ++ ")"
pp_show (TextBeside s _ p) = "(TB \"" ++ s ++ "\" " ++ pp_show p ++ ")"
pp_show (Nest k p) = "(N " ++ show IBOX(k) ++ " " ++ pp_show p ++ ")"
pp_show (Union p q) = pp_show p ++ "\nUNION\n" ++ pp_show q
pp_show NoDoc = "NoDoc"

p_show :: Doc -> Doc
p_show NoDoc = text "NoDoc"
p_show Empty = text "Empty"
p_show (NilAbove p) = sep [text "NA", nest 4 (p_show p)]
p_show (TextBeside s _ p) = sep [text "TB \"" <> text s <> text "\"", nest 4 (p_show p)]
p_show (Nest k p) = sep [text "N" <+> text (show IBOX(k)), nest 4 (p_show p)]
p_show (Union p q) = sep [text "(" <> p_show p <> text ")", 
			  text "UNION", 
			  text "(" <> p_show q <> text ")"]
\end{code}


*********************************************************
*							*
\subsection{@empty@, @text@, @nest@, @union@}
*							*
*********************************************************

\begin{code}
empty = Empty

text s = case length s of {IBOX(sl) -> TextBeside s sl Empty}

nest IBOX(k)  p = nest_ k p	-- No invariant to maintain for Nest

nest_ ILIT(0) p = p			-- Worth a try!
nest_ k       p = Nest k p

union a b = Union (ASSERT( valid_union_arg a ) a)	-- Note the ASSERT placement; not checked
		  (ASSERT( valid_union_arg b ) b)	-- unless the arg is going to be evald anyway
	  where
	    valid_union_arg (TextBeside _ _ _) = True
	    valid_union_arg (NilAbove _)       = True
	    valid_union_arg (Union _ _)        = True
	    valid_union_arg other	       = False
\end{code}

*********************************************************
*							*
\subsection{Vertical composition @$$@}
*							*
*********************************************************


\begin{code}
p1 $$ p2 = aboveNest p1 ILIT(0) p2

aboveNest :: Doc -> INT -> Doc -> Doc
-- Specfication: aboveNest p k q = p $$ (nest k q)
aboveNest NoDoc            k q = NoDoc
aboveNest Empty               k q = nest_ k q
aboveNest (NilAbove p)        k q = NilAbove (aboveNest p k q)
aboveNest (TextBeside s sl p) k q = TextBeside s sl (aboveNest1 p (k MINUS sl) q)
aboveNest (Nest k1 p)         k q = Nest k1 (aboveNest p (k MINUS k1) q)
aboveNest (p1 `Union` p2)     k q = aboveNest p1 k q `union` 
				    aboveNest p2 k q

-- Specification: text s <> aboveNest1 p k q 
--		= text s <> ((text "" <> p) $$ nest k q)
aboveNest1 (Nest _ p) k q = aboveNest1 p k q
aboveNest1 Empty      k q = nilAboveNest k q
aboveNest1 p          k q = aboveNest p k q
	-- All other p's guaranteed non-empty

-- Specification: text s <> nilAboveNest k q 
--		= text s <> (text "" $$ nest k q)
nilAboveNest k Empty = Empty	-- Here's why the "text s <>" is in the spec!
nilAboveNest k (Nest k1 q) = nilAboveNest  (k PLUS k1) q
nilAboveNest k NoDoc = NoDoc
nilAboveNest k q | k GR ILIT(0)		-- No newline if no overlap
		  = TextBeside (spaces k) k q
		  | otherwise		-- Put them really above
		  = NilAbove (nest_ k q)
\end{code}

Justification for aboveNest1:

\begin{verbatim}
	an1 (nest _ p) k q
		= (text "" <> nest _ p) $$ nest k q
	<7>	= (text "" <> p) $$ nest k q
		= an1 p k q
	 
	an1 empty k q
		= (test "" <> empty) $$ nest k q
	<6>	= test "" $$ nest k q
	 
	an1 (text s <> p) k q
		= (text "" <> text s <> p) $$ nest k q
	<4,14>	= (text s <> p) $$ nest k q
	 
	an1 (text "" $$ p) k q
		= (text "" <> (text "" $$ p)) $$ nest k q
	<8r>	= (text "" <> ((empty <> text "") $$ p)) $$ nest k q
	<3r,6>	= (text "" $$ p) $$ nest k q
\end{verbatim}

For union we note that TB, NA can be args of union.


*********************************************************
*							*
\subsection{Horizontal composition @<>@}
*							*
*********************************************************

\begin{code}
NoDoc            <> q = NoDoc
Empty               <> p = deNest p
(NilAbove p)        <> q = NilAbove (p <> q)
(TextBeside s sl p) <> q = TextBeside s sl (p <> q)
(Nest k p)          <> q = Nest k (p <> q)
(p1 `Union` p2)     <> q = (p1 <> q) `union` (p2 <> q)

deNest (Nest _ p) = p
deNest p          = p
\end{code}

The @<#>@ operator is only used internally.
It's interesting feature is that it absorbs an Empty on the left and
the right.
Its laws are:
\begin{verbatim}
	empty <#> q = q
	p <#> empty = p
	nest k p <#> q = nest k (p <#> q)
	p <#> nest k q = p <#> q, if p non-empty
	p <#> q = text s <> text " " <> q, if p,q non-empty

WE ASSUME THAT
	text "" <> g = g
which is true provided g isn't a nest.
\end{verbatim}
Like all the other operators it distributes over union in the usual way.

\begin{code}
NoDoc      <#> q = NoDoc
Empty	   <#> q = q
(Nest k p) <#> q = Nest k (p <#> q)
p 	   <#> q = p <> prefixSP q	-- p non-empty
\end{code}

prefixSP puts a space on the front of a non-empty document.

\begin{code}
prefixSP :: Doc -> Doc
prefixSP NoDoc = NoDoc
prefixSP (Nest k p) = prefixSP p
prefixSP Empty = Empty
prefixSP p = TextBeside " " ILIT(1) p
\end{code}


*********************************************************
*							*
\subsection{Separate, @sep@, Hughes version}
*							*
*********************************************************

\begin{code}
-- Specification: sep ps  = oneLiner (hsep ps)
--			   `union`
--			    vcat ps

sep = sepX True		-- Separate with spaces
cat = sepX False		-- Don't

sepX x []     = empty
sepX x (p:ps) = sep1 x p ILIT(0) ps


-- Specification: sep1 x k ys = sep (x : map (nest k) ys)
--			      = oneLiner (x <#> nest k (hsep ys))
--				`union` x $$ nest k (vcat ys)

sep1 x NoDoc 		   k ys = NoDoc
sep1 x Empty               k ys = nest_ k (sepX x ys)
sep1 x (NilAbove p)        k ys = NilAbove (aboveNest p k (vcat ys))
sep1 x (TextBeside s sl p) k ys = TextBeside s sl (sepNB x p (k MINUS sl) ys)
sep1 x (Nest n p)          k ys = Nest n (sep1 x p (k MINUS n) ys)
sep1 x (p `Union` q)       k ys = sep1 x p k ys
				`union`
				 (q $$ nest_ k (vcat ys))

-- Specification: sepNB p k ys = sep1 (text "" <> p) k ys
-- Called when we have already found some text in the first item

sepNB x NoDoc k ys	  = NoDoc
sepNB x Empty k ys        = oneLiner (if x then prefixSP (hsep ys)
					   else hcat ys)
			    `union` 
			    nilAboveNest k (vcat ys)
sepNB x (Nest _ p)  k ys  = sepNB x p k ys
sepNB x p k ys		  = sep1 x p k ys
\end{code}

\begin{verbatim}
sep1 g Empty k ys
	= oneLiner (Empty <g> foldr1 (<g>) (map (nest k) ys))
	  `union`
          Empty $$ nest k (foldr1 ($$) ys)
	= oneLiner (foldr1 (<g>) (map (nest k) ys))
	  `union`
	  nest k (foldr1 ($$) ys)

{Case ys = (y1:ys1), ys1 non-empty}
	= oneLiner (nest k y1 <g> foldr1 (<g>) (map (nest k) ys1))
	  `union`
	  nest k y1 $$ nest k (foldr1 ($$) ys1)
	= sep1 g (nest k y1) k ys1

{Case ys = [y1]}
	= oneLiner (nest k y1) `union` nest k y1
	= nest k y1


sepNB g Empty k ys
	= sep1 g (text "" <> Empty) k ys
	= sep1 g (text "") k ys
	= oneLiner (text "" <g> foldr1 (<g>) (map (nest k) ys))
	  `union`
	  text "" $$ nest k (foldr1 ($$) ys)
{Assumption about g}
	= oneLiner (g <> foldr1 <g> ys)
	  `union`
	  text "" $$ nest k (foldr1 ($$) ys)

\end{verbatim}

*********************************************************
*							*
\subsection{@fill@}
*							*
*********************************************************

\begin{code}
fsep = fill True
fcat = fill False

-- Specification: fill []  = empty
--		   fill (p:ps) = oneLiner p <#> oneLiner (hsep ps)
--			   `union`
--			    p $$ fill ps

fill x []     = empty
fill x (p:ps) = fill1 x p ILIT(0) ps


-- Specification: sep1 x k ys = sep (x : map (nest k) ys)
--			      = oneLiner (x <#> nest k (hsep ys))
--				`union` x $$ nest k (vcat ys)
--
fill1 x NoDoc	 	    k ys = NoDoc
fill1 x Empty               k ys = nest_ k (fill x ys)
fill1 x (NilAbove p)        k ys = NilAbove (aboveNest p k (fill x ys))
fill1 x (TextBeside s sl p) k ys = TextBeside s sl (fillNB x p (k MINUS sl) ys)
fill1 x (Nest n p)          k ys = Nest n (fill1 x p (k MINUS n) ys)
fill1 x (p `Union` q)       k ys = fill1 x p k ys
				   `union`
				   (q $$ nest_ k (fill x ys))

fillNB x NoDoc k ys	   = NoDoc
fillNB x Empty k []        = Empty
fillNB x Empty k (y:ys)    = (if x then prefixSP (fill1 x (oneLiner y) (k MINUS ILIT(1)) ys)
			           else           fill1 x (oneLiner y) k         ys)
			     `union` 
			     nilAboveNest k (fill x (y:ys))
fillNB x (Nest _ p)  k ys  = fillNB x p k ys
fillNB x p k ys		   = fill1 x p k ys
\end{code}


*********************************************************
*							*
\subsection{Selecting the best layout}
*							*
*********************************************************

\begin{code}
best :: Mode
     -> Int		-- Line length
     -> Int		-- Ribbon length
     -> Doc
     -> Doc		-- No unions in here!

best OneLineMode IBOX(w) IBOX(r) p
  = get p
  where
    get Empty               = Empty
    get NoDoc               = NoDoc
    get (NilAbove p)        = NilAbove (get p)
    get (TextBeside s sl p) = TextBeside s sl (get p)
    get (Nest k p)          = get p		-- Elide nest
    get (p `Union` q)       = first (get p) (get q)

best mode IBOX(w) IBOX(r) p
  = get w p
  where
    get :: INT		-- (Remaining) width of line
        -> Doc -> Doc
    get w Empty               = Empty
    get w NoDoc               = NoDoc
    get w (NilAbove p)        = NilAbove (get w p)
    get w (TextBeside s sl p) = TextBeside s sl (get1 w sl p)
    get w (Nest k p)          = Nest k (get (w MINUS k) p)
    get w (p `Union` q)       = nicest w r (get w p) (get w q)

    get1 :: INT		-- (Remaining) width of line
         -> INT		-- Amount of first line already eaten up
         -> Doc		-- This is an argument to TextBeside => eat Nests
         -> Doc		-- No unions in here!

    get1 w sl Empty               = Empty
    get1 w sl NoDoc               = NoDoc
    get1 w sl (NilAbove p)        = NilAbove (get (w MINUS sl) p)
    get1 w sl (TextBeside t tl p) = TextBeside t tl (get1 w (sl PLUS tl) p)
    get1 w sl (Nest k p)          = get1 w sl p
    get1 w sl (p `Union` q)       = nicest1 w r sl (get1 w sl p) 
				                   (get1 w sl q)

nicest w r p q = nicest1 w r ILIT(0) p q
nicest1 w r sl p q | fits ((w `minn` r) MINUS sl) p = p
		   | otherwise 	                 = q

fits :: INT	-- Space available
     -> Doc
     -> Bool	-- True if *first line* of Doc fits in space available
 
fits n p    | n LT ILIT(0) = False
fits n NoDoc               = False
fits n Empty               = True
fits n (NilAbove _)        = True
fits n (TextBeside _ sl p) = fits (n MINUS sl) p

minn x y | x LT y    = x
	 | otherwise = y
\end{code}

@first@ and @nonEmptySet@ are similar to @nicest@ and @fits@, only simpler.
@first@ returns its first argument if it is non-empty, otherwise its second.

\begin{code}
first p q | nonEmptySet p = p 
	  | otherwise     = q

nonEmptySet NoDoc           = False
nonEmptySet (p `Union` q)      = True
nonEmptySet Empty	       = True
nonEmptySet (NilAbove p)       = True		-- NoDoc always in first line
nonEmptySet (TextBeside _ _ p) = nonEmptySet p
nonEmptySet (Nest _ p)         = nonEmptySet p
\end{code}

@oneLiner@ returns the one-line members of the given set of @Doc@s.

\begin{code}
oneLiner :: Doc -> Doc
oneLiner NoDoc            = NoDoc
oneLiner Empty               = Empty
oneLiner (NilAbove p)        = NoDoc
oneLiner (TextBeside s sl p) = TextBeside s sl (oneLiner p)
oneLiner (Nest k p)          = Nest k (oneLiner p)
oneLiner (p `Union` pq)      = oneLiner p
\end{code}



*********************************************************
*							*
\subsection{Displaying the best layout}
*							*
*********************************************************


\begin{code}
#if defined(__HASKELL_1_3__)
renderStyle Style{mode, lineLength, ribbonsPerLine} doc 
  = fullRender mode lineLength ribbonsPerLine doc ""
#endif

showDoc doc cont = fullRender PageMode 100 2.5 doc cont
render doc       = fullRender PageMode 100 2.5 doc ""
\end{code}


\begin{code}
fullRender mode line_length ribbons_per_line doc cont
  = display mode line_length ribbon_length best_doc cont
  where 
    best_doc = best mode hacked_line_length ribbon_length doc

    hacked_line_length, ribbon_length :: Int
    ribbon_length = round (fromInt line_length / ribbons_per_line)
    hacked_line_length = case mode of { ZigZagMode -> MAXINT; other -> line_length }

display OneLineMode _ _ doc cont = easy_display ' '  doc cont
display LeftMode    _ _ doc cont = easy_display '\n' doc cont

display mode IBOX(page_width) IBOX(ribbon_width) doc cont
  = case page_width MINUS ribbon_width of { gap_width ->
    case gap_width DIV ILIT(2) of { shift ->
    let
    	lay k (Nest k1 p)  = lay (k PLUS k1) p
    	lay k Empty        = cont
    
    	lay k (NilAbove p) = '\n' : lay k p
    
    	lay k (TextBeside s sl p)
	    = case mode of
		    ZigZagMode |  k GREQ gap_width
			       -> '\n' : multi_ch shift '/' (
				  '\n' : lay1 (k MINUS shift) s sl p)

			       |  k LT ILIT(0)
			       -> '\n' : multi_ch shift '\\' (
				  '\n' : lay1 (k PLUS shift) s sl p )

		    other -> lay1 k s sl p
    
    	lay1 k s sl p = indent k (s ++ (lay2 (k PLUS sl) p))
    
    	lay2 k (NilAbove p)        = '\n' : lay k p
    	lay2 k (TextBeside s sl p) = s ++ (lay2 (k PLUS sl) p)
    	lay2 k (Nest _ p)          = lay2 k p
    	lay2 k Empty               = cont
    in
    lay ILIT(0) doc
    }}

easy_display nl_char doc cont 
  = lay doc
  where
    lay (Nest k p)   	    = lay p
    lay Empty        	    = cont
    lay (NilAbove p) 	    = nl_char : lay p
    lay (TextBeside s sl p) = s ++ lay p

indent n rest | n GREQ ILIT(8) = '\t' : indent (n MINUS ILIT(8)) rest
	      | otherwise = multi_ch n ' ' rest

multi_ch ILIT(0) ch rest = rest
multi_ch n       ch rest = ch : multi_ch (n MINUS ILIT(1)) ch rest

spaces ILIT(0) = ""
spaces n       = ' ' : spaces (n MINUS ILIT(1))
\end{code}

