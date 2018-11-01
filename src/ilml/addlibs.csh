#! /bin/csh -f
set files = `cat $1/lib.index | grep -v '^_NS'`
foreach f ($files)
	set a = (`(cat $1/parsed/$f.p; echo '') | sed -n 's/^f#\([^	]*\).*#\([0-9]*\)	#-*[0-9]*	.*$/\1 \2/p'`)
	if ($#a == 2) then
		set name = C$a[1]
		echo $name
		set ar = $a[2]
		if ($ar != -1) then
			echo V$name
			echo J$ar$name
			echo S$ar$name
		endif
	else
		(cat $1/parsed/$f.p; echo '') | ../bin/mygreps '^f#'
		if ($status) then
			echo "Bad function $f" >/dev/tty
			exit 1
		else
			echo C$f
		endif
	endif
end
