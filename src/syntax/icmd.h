#ifndef icmd_defined
#define icmd_defined
typedef enum {
	Iexpr,
	Ibinding,
	Itload,
	Icload,
	Imload,
	Imsg,
	Imsgnp,
	Inull,
	Iwhatis,
	Ihelp,
	Ishow_,
	Imbind
} Ticmd;

typedef struct { Ticmd tag; } *icmd;

/* Compatibility defines */
extern Ticmd ticmd();

#endif
extern icmd mkIexpr();
extern tree *RgIexpr();
#define gIexpr(xyzxyz) (*RgIexpr(xyzxyz))

extern icmd mkIbinding();
extern list *RgIbinding();
#define gIbinding(xyzxyz) (*RgIbinding(xyzxyz))

extern icmd mkItload();
extern list *RgItloads();
#define gItloads(xyzxyz) (*RgItloads(xyzxyz))

extern icmd mkIcload();
extern list *RgIcloadnames();
#define gIcloadnames(xyzxyz) (*RgIcloadnames(xyzxyz))
extern tree *RgIcloadimp();
#define gIcloadimp(xyzxyz) (*RgIcloadimp(xyzxyz))

extern icmd mkImload();
extern id *RgImloadfile();
#define gImloadfile(xyzxyz) (*RgImloadfile(xyzxyz))
extern tree *RgImloadtree();
#define gImloadtree(xyzxyz) (*RgImloadtree(xyzxyz))

extern icmd mkImsg();
extern tree *RgImsg();
#define gImsg(xyzxyz) (*RgImsg(xyzxyz))

extern icmd mkImsgnp();
extern tree *RgImsgnp();
#define gImsgnp(xyzxyz) (*RgImsgnp(xyzxyz))

extern icmd mkInull();

extern icmd mkIwhatis();
extern id *RgIwhatid();
#define gIwhatid(xyzxyz) (*RgIwhatid(xyzxyz))

extern icmd mkIhelp();

extern icmd mkIshow_();
extern id *RgIshow_();
#define gIshow_(xyzxyz) (*RgIshow_(xyzxyz))

extern icmd mkImbind();
extern tree *RgImbpat();
#define gImbpat(xyzxyz) (*RgImbpat(xyzxyz))
extern tree *RgImbexp();
#define gImbexp(xyzxyz) (*RgImbexp(xyzxyz))

