#ifndef stmt_defined
#define stmt_defined
typedef enum {
	stmtexp,
	stmtexpstmt,
	stmtlet,
	stmtbind
} Tstmt;

typedef struct { Tstmt tag; } *stmt;

/* Compatibility defines */
extern Tstmt tstmt();

#endif
extern stmt mkstmtexp();
extern tree *Rgseexp();
#define gseexp(xyzxyz) (*Rgseexp(xyzxyz))

extern stmt mkstmtexpstmt();
extern tree *Rgsesexp();
#define gsesexp(xyzxyz) (*Rgsesexp(xyzxyz))
extern stmt *Rgsesstmt();
#define gsesstmt(xyzxyz) (*Rgsesstmt(xyzxyz))

extern stmt mkstmtlet();
extern binding *Rgslbind();
#define gslbind(xyzxyz) (*Rgslbind(xyzxyz))
extern stmt *Rgslstmt();
#define gslstmt(xyzxyz) (*Rgslstmt(xyzxyz))

extern stmt mkstmtbind();
extern tree *Rgsbpat();
#define gsbpat(xyzxyz) (*Rgsbpat(xyzxyz))
extern tree *Rgsbexp();
#define gsbexp(xyzxyz) (*Rgsbexp(xyzxyz))
extern stmt *Rgsbstmt();
#define gsbstmt(xyzxyz) (*Rgsbstmt(xyzxyz))

