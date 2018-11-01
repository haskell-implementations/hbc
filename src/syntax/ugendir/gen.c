#include <stdio.h>
#include "id.h"
#include "tree.h"
extern FILE *fh, *fc;

ge_typdef(t)
    tree t;
{
	/*
	** Generate ( to the .h - file)
	** 	typdef enum {
	**		constructor1,
	**		constructor2,
	**		...
	**	} *typname;
	*/
	fprintf(fh, "#ifndef %s_defined\n", gtid(t));
	fprintf(fh, "#define %s_defined\n", gtid(t));
	fprintf(fh, "typedef enum {\n");
	ge_typlist(gtdeflist(t));
	fprintf(fh, "\n} T%s;\n\n", gtid(t));
	/*
	** Generate (to the .h - file)
	**	typedef struct { Ttypename tag; } *typename;
	*/
	fprintf(fh, "typedef struct { T%s tag; } *%s;\n", gtid(t), gtid(t));
	fprintf(fh, "\n/* Compatibility defines */\n");
	fprintf(fh, "extern T%s t%s();\n\n", gtid(t), gtid(t));
	fprintf(fh, "#endif\n");
}

ge_typlist(t)
    tree t;
{
	switch(ttree(t)) {
	  case deflist:
		ge_typlist(gdeflist(t));
		fprintf(fh, ",\n\t%s", gdid(gdef(t)));
		break;
	  case def:
		fprintf(fh, "\t%s", gdid(t));
		break;
	  default:
		printf("ge_typlist: funny abstract syntax.\n");
		break;
	}
}

gs_typlist(t, tid)
    tree t;
    id tid;
{
	switch(ttree(t)) {
	  case deflist:
		gs_typlist(gdeflist(t), tid);
		gs_def(gdef(t), tid);
		break;
	  case def:
		gs_def(t, tid);
		break;
	  default:
		printf("gs_typlist: funny abstract syntax.\n");
		break;
	}
}

gs_def(t, tid)
   tree t;
   id tid;
{
	fprintf(fc, "struct S%s {\n", gdid(t));
	fprintf(fc, "\tT%s tag;\n", tid);
	gs_itemlist(gditemlist(t));
	fprintf(fc, "};\n\n");
}

gs_itemlist(t)
    tree t;
{
	switch(ttree(t)) {
	  case emitemlist:
		break;
	  case itemlist:
		gs_itemlist(gitemlist(t));
		fprintf(fc, "\t%s X%s;\n",
			gitemtypid(gitem(t)), gitemfunid(gitem(t)) );
		break;
	  case item:
		fprintf(fc, "\t%s X%s;\n", 
			gitemtypid(t), gitemfunid(t));
		break;
	  default:
		printf("gs_itemlist: funny abs. syntax: %d\n.", ttree(t));
		break;
	}
}

g_tagfun(typid)
    id typid;
{
	fprintf(fc, "T%s t%s(t)\n %s t;\n{\n\treturn(t -> tag);\n}\n\n",
		    typid, typid, typid);
}
/*******************************************************************/

g_consels(t, typid)
    tree t;
    id typid;
{
	switch(ttree(t)) {
	  case deflist:
		g_consels(gdeflist(t), typid);
		g_typconsel(gdef(t), typid);
		break;
	  case def:
		g_typconsel(t, typid);
		break;
	  default:
		printf("g_consel: funny abstract syntax.\n");
		break;
	}
}

/***********************************************************************/

g_typconsel(t, typid)
    tree t;
    id typid;
{
	fprintf(fc, "\n/************** %s ******************/\n\n", gdid(t));
	gencons(typid, t);
	gensels(typid, gdid(t), gditemlist(t));
	fprintf(fh, "\n");
}

gencons(typid, t)
  id typid;
  tree t; /* of kind 'def'. */
{
	fprintf(fh, "extern %s mk%s();\n", typid, gdid(t));

	fprintf(fc, "%s mk%s(", typid, gdid(t));
	genmkparamlist(gditemlist(t));
	fprintf(fc, ")\n");
	genmkparamdekl(gditemlist(t));
	fprintf(fc, "{\n\tregister struct S%s *pp =\n", gdid(t));
	fprintf(fc, "\t\t(struct S%s *) malloc(sizeof(struct S%s));\n",
		    gdid(t), gdid(t));
	fprintf(fc, "\tpp -> tag = %s;\n", gdid(t));
	genmkfillin(gditemlist(t));
	fprintf(fc, "\treturn((%s)pp);\n", typid);
	fprintf(fc, "}\n");
}

genmkparamlist(t)
   tree t;
{
	switch(ttree(t)) {
	  case emitemlist:
		break;
	  case itemlist:
		genmkparamlist(gitemlist(t));
		fprintf(fc, ", ");
		genmkparamlist(gitem(t));
		break;
	  case item:
		fprintf(fc, "PP%s", gitemfunid(t));
		break;
	  default:
		printf("genparamlist: funny abs syntax.\n");
		break;
	}
}

genmkparamdekl(t)
   tree t; /* of kind 'itemlist' or 'item' */
{
	switch(ttree(t)) {
	  case emitemlist:
		break;
	  case itemlist:
		genmkparamdekl(gitemlist(t));
		genmkparamdekl(gitem(t));
		break;
	  case item:
		fprintf(fc, " %s PP%s;\n", gitemtypid(t), gitemfunid(t));
		break;
	  default:
		printf("genmkparamdekl: funny abs syntax.\n");
		break;
	}
}

genmkfillin(t)
    tree t;
{
	switch(ttree(t)) {
	  case emitemlist:
		break;
	  case itemlist:
		genmkfillin(gitemlist(t));
		genmkfillin(gitem(t));
		break;
	  case item:
		fprintf(fc, "\tpp -> X%s = PP%s;\n", 
			gitemfunid(t), gitemfunid(t));
		break;
	}
}

gensels(typid, variantid, t)
    id typid;
    id variantid;
    tree t;
{
	switch(ttree(t)) {
	  case emitemlist:
		break;
	  case itemlist:
		gensels(typid, variantid, gitemlist(t));
		gensels(typid, variantid, gitem(t));
		break;
	  case item:
		fprintf(fc, "\n%s *R%s(t)\n struct S%s *t;\n{\n", 
			     gitemtypid(t), gitemfunid(t), variantid);
		fprintf(fh,
		  "extern %s *R%s();\n#define %s(xyzxyz) (*R%s(xyzxyz))\n",
		  gitemtypid(t), gitemfunid(t), gitemfunid(t), gitemfunid(t));
		fprintf(fc, "\tif(t -> tag != %s)\n", variantid);
		fprintf(fc, "\t\tprintf(\"%s: illegal selection; was %%d\\n\", t -> tag);\n", gitemfunid(t));
		fprintf(fc, "\treturn(& t -> X%s);\n}\n", gitemfunid(t));
		break;
	  default:
		printf("gensels: funny abs syntax.\n");
		break;
	}

}
