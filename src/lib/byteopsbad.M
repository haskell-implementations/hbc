	.data
	.export	_badbytes
_badbytes:
	.word	PAIR0,ap1,ap1
	.word	0	
ap1:	.word	APG,CPfail,stri
stri:	.word	STRING,cstr,0
cstr:	.string	"Bad conversion in Bytes"
	.text
