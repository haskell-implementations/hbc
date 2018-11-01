%{
# include "id.h"
# include "tree.h"
extern tree root;
int yylineno;
%}
%token	ID TYPE SEMICOL COLON END STDEF ENDDEF

%union {
    tree utree;
    id uid;
}

%type <utree> deflist def item itemlist
%type <uid> ID

%%

typdef	: 
	TYPE ID SEMICOL deflist END SEMICOL =
	{
		root = mktypdef($2, $4);
	};

deflist	:
	def =
	{
		$$ = $1;
	} |
	deflist def =
	{
		$$ = mkdeflist($1, $2);
	};

def	:
	ID COLON STDEF itemlist ENDDEF SEMICOL =
	{
		$$ = mkdef($1, $4);
	} |
	ID COLON STDEF ENDDEF SEMICOL =
	{
		$$ = mkdef($1, mkemitemlist());
	};

itemlist:
	item =
	{
		$$ = $1;
	} |
	itemlist item =
	{
		$$ = mkitemlist($1, $2);
	};

item	:
	ID COLON ID SEMICOL =
	{
		$$ = mkitem($1, $3);
	};
