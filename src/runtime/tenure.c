#include "runtime.h"
#include "vars.h"
#include "../mcode/limit.h"


/* Updateable */
extern Tag AP  ,VAP  ,ZAP  ,HOLE,  STRING  ,STRINGN  ,INPUTD  ,INPUT  ;  /* created by runtime */
extern Tag                         STRING_F,STRINGN_F,INPUTD_F,INPUT_F;  /* Should not do writeupdate */
extern Tag AP_1,VAP_1,ZAP_1,HOLE_1,STRING_1,STRINGN_1,INPUTD_1,INPUT_1;
extern Tag AP_2,VAP_2,ZAP_2,HOLE_2,STRING_2,STRINGN_2,INPUTD_2,INPUT_2;
extern Tag APG ,VAPG                                                  ;

/* Pointer */
extern Tag CAP  ,BIGNUM  ,PAIR  ,PAIR0  ,PAIR1  ,PAIR2  ,PAIR3  ,PAIR3  ,PAIR4  ,VEK  ;
extern Tag CAP_1,BIGNUM_1,PAIR_1,PAIR0_1,PAIR1_1,PAIR2_1,PAIR3_1,PAIR3_1,PAIR4_1,VEK_1;
extern Tag CAP_2,BIGNUM_2,PAIR_2,PAIR0_2,PAIR1_2,PAIR2_2,PAIR3_2,PAIR3_2,PAIR4_2,VEK_2;

/* Word */
extern Tag CHAR  ,INT  ,SFLOAT  ,DFLOAT  ,TAG  ,TAG0  ,DVEK  ;
extern Tag CHAR_1,INT_1,SFLOAT_1,DFLOAT_1,TAG_1,TAG0_1,DVEK_1;
extern Tag CHAR_2,INT_2,SFLOAT_2,DFLOAT_2,TAG_2,TAG0_2,DVEK_2;

extern void g2n11();
extern void g2n12();
extern void g2n20();
extern void g2n21();
extern void g2n30();
extern void g2nvap();
extern void g2nzap();
extern void g2n30();
extern void g2ninp();
extern void g2nind();
extern void g2nvek();
extern void g2ndvek();
extern void g2nchr();
extern void g2o11();
extern void g2o12();
extern void g2o20();
extern void g2o21();
extern void g2o30();
extern void g2ovap();
extern void g2ozap();
extern void g2o30();
extern void g2oinp();
extern void g2oind();
extern void g2ovek();
extern void g2odvek();
extern void g2ochr();


void setuptenure(update,pointer,word)
int update,pointer,word;
{
  if(AP.gen2Copy != FUNPTR(g2n12)) {
    fprintf(stderr,"gen2Copy didn't work as expected %08x != %08x\n",AP.gen2Copy,FUNPTR(g2n12));
    exit(-1);
  }
  if(AP.gen2Tag != &AP_1) {
    fprintf(stderr,"gen2Tag didn't work as expected %08x != %08x\n",AP.gen2Tag,&AP_1);
    exit(-1);
  }

  if(update>=0) {
    if(update == 9) {/* Never tenure */
      AP.gen2Copy        = FUNPTR(g2n12);  AP.gen2Tag        = &AP;
      APG.gen2Copy       = FUNPTR(g2n12);  APG.gen2Tag       = &APG;
      VAP.gen2Copy       = FUNPTR(g2nvap); VAP.gen2Tag       = &VAP;
      VAPG.gen2Copy      = FUNPTR(g2nvap); VAPG.gen2Tag      = &VAPG;
      ZAP.gen2Copy       = FUNPTR(g2nzap); ZAP.gen2Tag       = &ZAP;
      HOLE.gen2Copy      = FUNPTR(g2n30);  HOLE.gen2Tag      = &HOLE;
      STRING.gen2Copy    = FUNPTR(g2n30);  STRING.gen2Tag    = &STRING;
      STRING_F.gen2Copy  = FUNPTR(g2n30);  STRING_F.gen2Tag  = &STRING;
      STRINGN.gen2Copy   = FUNPTR(g2n30);  STRINGN.gen2Tag   = &STRINGN;
      STRINGN_F.gen2Copy = FUNPTR(g2n30);  STRINGN_F.gen2Tag = &STRINGN;
      INPUT.gen2Copy     = FUNPTR(g2ninp); INPUT.gen2Tag     = &INPUT;
      INPUT_F.gen2Copy   = FUNPTR(g2ninp); INPUT_F.gen2Tag   = &INPUT;
      INPUTD.gen2Copy    = FUNPTR(g2nind); INPUTD.gen2Tag    = &INPUTD;
      INPUTD_F.gen2Copy  = FUNPTR(g2nind); INPUTD_F.gen2Tag  = &INPUTD;
	AP_1.gen2Copy        = FUNPTR(g2n12);  AP_1.gen2Tag        = &AP;
	VAP_1.gen2Copy       = FUNPTR(g2nvap); VAP_1.gen2Tag       = &VAP;
	ZAP_1.gen2Copy       = FUNPTR(g2nzap); ZAP_1.gen2Tag       = &ZAP;
	HOLE_1.gen2Copy      = FUNPTR(g2n30);  HOLE_1.gen2Tag      = &HOLE;
	STRING_1.gen2Copy    = FUNPTR(g2n30);  STRING_1.gen2Tag    = &STRING;
	STRINGN_1.gen2Copy   = FUNPTR(g2n30);  STRINGN_1.gen2Tag   = &STRINGN;
	INPUT_1.gen2Copy     = FUNPTR(g2ninp); INPUT_1.gen2Tag     = &INPUT;
	INPUTD_1.gen2Copy    = FUNPTR(g2nind); INPUTD_1.gen2Tag    = &INPUTD;
    } else {
      if(update == 0) { /* Tenure immediately */
	AP.gen2Copy        = FUNPTR(g2o12);  AP.gen2Tag        = &APG;
	APG.gen2Copy       = FUNPTR(g2o12);  APG.gen2Tag       = &APG;
	VAP.gen2Copy       = FUNPTR(g2ovap); VAP.gen2Tag       = &VAPG;
	VAPG.gen2Copy      = FUNPTR(g2ovap); VAPG.gen2Tag      = &VAPG;
	ZAP.gen2Copy       = FUNPTR(g2ozap); ZAP.gen2Tag       = &ZAP;
	HOLE.gen2Copy      = FUNPTR(g2o30);  HOLE.gen2Tag      = &HOLE;
	STRING.gen2Copy    = FUNPTR(g2o30);  STRING.gen2Tag    = &STRING;
	STRING_F.gen2Copy  = FUNPTR(g2o30);  STRING_F.gen2Tag  = &STRING;
	STRINGN.gen2Copy   = FUNPTR(g2o30);  STRINGN.gen2Tag   = &STRINGN;
	STRINGN_F.gen2Copy = FUNPTR(g2o30);  STRINGN_F.gen2Tag = &STRINGN;
	INPUT.gen2Copy     = FUNPTR(g2oinp); INPUT.gen2Tag     = &INPUT;
	INPUT_F.gen2Copy   = FUNPTR(g2oinp); INPUT_F.gen2Tag   = &INPUT;
	INPUTD.gen2Copy    = FUNPTR(g2oind); INPUTD.gen2Tag    = &INPUTD;
	INPUTD_F.gen2Copy  = FUNPTR(g2oind); INPUTD_F.gen2Tag  = &INPUTD;
      } else { /* survive at least one */
	AP.gen2Copy        = FUNPTR(g2n12);  AP.gen2Tag        = &AP_1;
	APG.gen2Copy       = FUNPTR(g2n12);  APG.gen2Tag       = &AP_1;
	VAP.gen2Copy       = FUNPTR(g2nvap); VAP.gen2Tag       = &VAP_1;
	VAPG.gen2Copy      = FUNPTR(g2nvap); VAPG.gen2Tag      = &VAP_1;
	ZAP.gen2Copy       = FUNPTR(g2nzap); ZAP.gen2Tag       = &ZAP_1;
	HOLE.gen2Copy      = FUNPTR(g2n30);  HOLE.gen2Tag      = &HOLE_1;
	STRING.gen2Copy    = FUNPTR(g2n30);  STRING.gen2Tag    = &STRING_1;
	STRING_F.gen2Copy  = FUNPTR(g2n30);  STRING_F.gen2Tag  = &STRING_1;
	STRINGN.gen2Copy   = FUNPTR(g2n30);  STRINGN.gen2Tag   = &STRINGN_1;
	STRINGN_F.gen2Copy = FUNPTR(g2n30);  STRINGN_F.gen2Tag = &STRINGN_1;
	INPUT.gen2Copy     = FUNPTR(g2ninp); INPUT.gen2Tag     = &INPUT_1;
	INPUT_F.gen2Copy   = FUNPTR(g2ninp); INPUT_F.gen2Tag   = &INPUT_1;
	INPUTD.gen2Copy    = FUNPTR(g2nind); INPUTD.gen2Tag    = &INPUTD_1;
	INPUTD_F.gen2Copy  = FUNPTR(g2nind); INPUTD_F.gen2Tag  = &INPUTD_1;
      }
      if(update == 1) { /* Tenure after 1 gc */
	AP_1.gen2Copy        = FUNPTR(g2o12);  AP_1.gen2Tag        = &APG;
	VAP_1.gen2Copy       = FUNPTR(g2ovap); VAP_1.gen2Tag       = &VAPG;
	ZAP_1.gen2Copy       = FUNPTR(g2ozap); ZAP_1.gen2Tag       = &ZAP;
	HOLE_1.gen2Copy      = FUNPTR(g2o30);  HOLE_1.gen2Tag      = &HOLE;
	STRING_1.gen2Copy    = FUNPTR(g2o30);  STRING_1.gen2Tag    = &STRING;
	STRINGN_1.gen2Copy   = FUNPTR(g2o30);  STRINGN_1.gen2Tag   = &STRINGN;
	INPUT_1.gen2Copy     = FUNPTR(g2oinp); INPUT_1.gen2Tag     = &INPUT;
	INPUTD_1.gen2Copy    = FUNPTR(g2oind); INPUTD_1.gen2Tag    = &INPUTD;
      } else {
	AP_1.gen2Copy        = FUNPTR(g2n12);  AP_1.gen2Tag        = &AP_2;
	VAP_1.gen2Copy       = FUNPTR(g2nvap); VAP_1.gen2Tag       = &VAP_2;
	ZAP_1.gen2Copy       = FUNPTR(g2nzap); ZAP_1.gen2Tag       = &ZAP_2;
	HOLE_1.gen2Copy      = FUNPTR(g2n30);  HOLE_1.gen2Tag      = &HOLE_2;
	STRING_1.gen2Copy    = FUNPTR(g2n30);  STRING_1.gen2Tag    = &STRING_2;
	STRINGN_1.gen2Copy   = FUNPTR(g2n30);  STRINGN_1.gen2Tag   = &STRINGN_2;
	INPUT_1.gen2Copy     = FUNPTR(g2ninp); INPUT_1.gen2Tag     = &INPUT_2;
	INPUTD_1.gen2Copy    = FUNPTR(g2nind); INPUTD_1.gen2Tag    = &INPUTD_2;
      }      
      if(update > 2) {
	fprintf(stderr,"Tenuring at age %d for nodes that can be updated is not supported\n",update);
	exit(-1);
      }
    }
  }

  if(pointer>=0) {
    if(pointer==9) { /* Never tenure */
	CAP.gen2Copy    = FUNPTR(g2n12); CAP.gen2Tag    = &CAP;
	BIGNUM.gen2Copy = FUNPTR(g2n11); BIGNUM.gen2Tag = &BIGNUM;
	TAG.gen2Copy    = FUNPTR(g2n21); TAG.gen2Tag    = &TAG;
	PAIR.gen2Copy   = FUNPTR(g2n12); PAIR.gen2Tag   = &PAIR;
	PAIR0.gen2Copy  = FUNPTR(g2n12); PAIR0.gen2Tag  = &PAIR0;
	PAIR1.gen2Copy  = FUNPTR(g2n12); PAIR1.gen2Tag  = &PAIR1;
	PAIR2.gen2Copy  = FUNPTR(g2n12); PAIR2.gen2Tag  = &PAIR2;
	PAIR3.gen2Copy  = FUNPTR(g2n12); PAIR3.gen2Tag  = &PAIR3;
	PAIR4.gen2Copy  = FUNPTR(g2n12); PAIR4.gen2Tag  = &PAIR4;
	VEK.gen2Copy    = FUNPTR(g2nvek); VEK.gen2Tag   = &VEK;
    } else {
      if(pointer == 0) { /* Immediate tenure */
	CAP.gen2Copy    = FUNPTR(g2o12); CAP.gen2Tag    = &CAP;
	BIGNUM.gen2Copy = FUNPTR(g2o11); BIGNUM.gen2Tag = &BIGNUM;
	TAG.gen2Copy    = FUNPTR(g2o21); TAG.gen2Tag    = &TAG;
	PAIR.gen2Copy   = FUNPTR(g2o12); PAIR.gen2Tag   = &PAIR;
	PAIR0.gen2Copy  = FUNPTR(g2o12); PAIR0.gen2Tag  = &PAIR0;
	PAIR1.gen2Copy  = FUNPTR(g2o12); PAIR1.gen2Tag  = &PAIR1;
	PAIR2.gen2Copy  = FUNPTR(g2o12); PAIR2.gen2Tag  = &PAIR2;
	PAIR3.gen2Copy  = FUNPTR(g2o12); PAIR3.gen2Tag  = &PAIR3;
	PAIR4.gen2Copy  = FUNPTR(g2o12); PAIR4.gen2Tag  = &PAIR4;
	VEK.gen2Copy    = FUNPTR(g2ovek); VEK.gen2Tag   = &VEK;
      } else {
	CAP.gen2Copy    = FUNPTR(g2n12); CAP.gen2Tag    = &CAP_1;
	BIGNUM.gen2Copy = FUNPTR(g2n11); BIGNUM.gen2Tag = &BIGNUM_1;
	TAG.gen2Copy    = FUNPTR(g2n21); TAG.gen2Tag    = &TAG_1;
	PAIR.gen2Copy   = FUNPTR(g2n12); PAIR.gen2Tag   = &PAIR_1;
	PAIR0.gen2Copy  = FUNPTR(g2n12); PAIR0.gen2Tag  = &PAIR0_1;
	PAIR1.gen2Copy  = FUNPTR(g2n12); PAIR1.gen2Tag  = &PAIR1_1;
	PAIR2.gen2Copy  = FUNPTR(g2n12); PAIR2.gen2Tag  = &PAIR2_1;
	PAIR3.gen2Copy  = FUNPTR(g2n12); PAIR3.gen2Tag  = &PAIR3_1;
	PAIR4.gen2Copy  = FUNPTR(g2n12); PAIR4.gen2Tag  = &PAIR4_1;
	VEK.gen2Copy    = FUNPTR(g2nvek); VEK.gen2Tag   = &VEK_1;
	VEK.gen2Copy    = FUNPTR(g2ovek); VEK.gen2Tag   = &VEK;

      }
      if(pointer == 1) {
	CAP_1.gen2Copy    = FUNPTR(g2o12); CAP_1.gen2Tag    = &CAP;
	BIGNUM_1.gen2Copy = FUNPTR(g2o11); BIGNUM_1.gen2Tag = &BIGNUM;
	TAG_1.gen2Copy    = FUNPTR(g2n21); TAG_1.gen2Tag    = &TAG;
	PAIR_1.gen2Copy   = FUNPTR(g2o12); PAIR_1.gen2Tag   = &PAIR;
	PAIR0_1.gen2Copy  = FUNPTR(g2o12); PAIR0_1.gen2Tag  = &PAIR0;
	PAIR1_1.gen2Copy  = FUNPTR(g2o12); PAIR1_1.gen2Tag  = &PAIR1;
	PAIR2_1.gen2Copy  = FUNPTR(g2o12); PAIR2_1.gen2Tag  = &PAIR2;
	PAIR3_1.gen2Copy  = FUNPTR(g2o12); PAIR3_1.gen2Tag  = &PAIR3;
	PAIR4_1.gen2Copy  = FUNPTR(g2o12); PAIR4_1.gen2Tag  = &PAIR4;
	VEK_1.gen2Copy    = FUNPTR(g2ovek); VEK_1.gen2Tag   = &VEK;
      } else {
	CAP_1.gen2Copy    = FUNPTR(g2n12); CAP_1.gen2Tag    = &CAP_2;
	BIGNUM_1.gen2Copy = FUNPTR(g2n11); BIGNUM_1.gen2Tag = &BIGNUM_2;
	TAG_1.gen2Copy    = FUNPTR(g2n21); TAG_1.gen2Tag    = &TAG_2;
	PAIR_1.gen2Copy   = FUNPTR(g2n12); PAIR_1.gen2Tag   = &PAIR_2;
	PAIR0_1.gen2Copy  = FUNPTR(g2n12); PAIR0_1.gen2Tag  = &PAIR0_2;
	PAIR1_1.gen2Copy  = FUNPTR(g2n12); PAIR1_1.gen2Tag  = &PAIR1_2;
	PAIR2_1.gen2Copy  = FUNPTR(g2n12); PAIR2_1.gen2Tag  = &PAIR2_2;
	PAIR3_1.gen2Copy  = FUNPTR(g2n12); PAIR3_1.gen2Tag  = &PAIR3_2;
	PAIR4_1.gen2Copy  = FUNPTR(g2n12); PAIR4_1.gen2Tag  = &PAIR4_2;
	VEK_1.gen2Copy    = FUNPTR(g2nvek); VEK_1.gen2Tag   = &VEK_2;
      }
      if(pointer > 2) {
	fprintf(stderr,"Tenuring at age %d for nodes that contain pointers is not supported\n",pointer);
	exit(-1);
      }
    }
  }

  if(word>=0) {
    if(word==9) { /* Never tenure */
	CHAR.gen2Copy   = FUNPTR(g2nchr);  CHAR.gen2Tag   = &CHAR;
	INT.gen2Copy    = FUNPTR(g2n20);   INT.gen2Tag    = &INT;
	SFLOAT.gen2Copy = FUNPTR(g2n20);   SFLOAT.gen2Tag = &SFLOAT;
	DFLOAT.gen2Copy = FUNPTR(g2n30);   DFLOAT.gen2Tag = &DFLOAT;
	TAG0.gen2Copy   = FUNPTR(g2n20);   TAG0.gen2Tag   = &TAG0;
	DVEK.gen2Copy   = FUNPTR(g2ndvek); DVEK.gen2Tag   = &DVEK;     
    } else {
      if(word == 0) { /* Immediate tenure */
	CHAR.gen2Copy   = FUNPTR(g2ochr);  CHAR.gen2Tag   = &CHAR;
	INT.gen2Copy    = FUNPTR(g2o20);   INT.gen2Tag    = &INT;
	SFLOAT.gen2Copy = FUNPTR(g2o20);   SFLOAT.gen2Tag = &SFLOAT;
	DFLOAT.gen2Copy = FUNPTR(g2o30);   DFLOAT.gen2Tag = &DFLOAT;
	TAG0.gen2Copy   = FUNPTR(g2o20);   TAG0.gen2Tag   = &TAG0;
	DVEK.gen2Copy   = FUNPTR(g2odvek); DVEK.gen2Tag   = &DVEK;   
      } else {
	CHAR.gen2Copy   = FUNPTR(g2nchr);  CHAR.gen2Tag   = &CHAR_1;
	INT.gen2Copy    = FUNPTR(g2n20);   INT.gen2Tag    = &INT_1;
	SFLOAT.gen2Copy = FUNPTR(g2n20);   SFLOAT.gen2Tag = &SFLOAT_1;
	DFLOAT.gen2Copy = FUNPTR(g2n30);   DFLOAT.gen2Tag = &DFLOAT_1;
	TAG0.gen2Copy   = FUNPTR(g2n20);   TAG0.gen2Tag   = &TAG0_1;
	DVEK.gen2Copy   = FUNPTR(g2ndvek); DVEK.gen2Tag   = &DVEK_1;   
      }
      if(word == 1) {
	CHAR_1.gen2Copy   = FUNPTR(g2ochr);  CHAR_1.gen2Tag   = &CHAR;
	INT_1.gen2Copy    = FUNPTR(g2o20);   INT_1.gen2Tag    = &INT;
	SFLOAT_1.gen2Copy = FUNPTR(g2o20);   SFLOAT_1.gen2Tag = &SFLOAT;
	DFLOAT_1.gen2Copy = FUNPTR(g2o30);   DFLOAT_1.gen2Tag = &DFLOAT;
	TAG0_1.gen2Copy   = FUNPTR(g2o20);   TAG0_1.gen2Tag   = &TAG0;
	DVEK_1.gen2Copy   = FUNPTR(g2odvek); DVEK_1.gen2Tag   = &DVEK;      
      } else {
	CHAR_1.gen2Copy   = FUNPTR(g2nchr);  CHAR_1.gen2Tag   = &CHAR_2;
	INT_1.gen2Copy    = FUNPTR(g2n20);   INT_1.gen2Tag    = &INT_2;
	SFLOAT_1.gen2Copy = FUNPTR(g2n20);   SFLOAT_1.gen2Tag = &SFLOAT_2;
	DFLOAT_1.gen2Copy = FUNPTR(g2n30);   DFLOAT_1.gen2Tag = &DFLOAT_2;
	TAG0_1.gen2Copy   = FUNPTR(g2n20);   TAG0_1.gen2Tag   = &TAG0_2;
	DVEK_1.gen2Copy   = FUNPTR(g2ndvek); DVEK_1.gen2Tag   = &DVEK_2;
      }
      if(word > 2) {
	fprintf(stderr,"Tenuring at age %d for nodes that contain words is not supported\n",word);
	exit(-1);
      }
    }
  }
}
