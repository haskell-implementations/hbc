module -- flag decoder
#include "../expr/id.t"		/* preludename */
export	Debug, IdDebug, Strict, Semistrict, Gflag, ImpDebug, Fullname, Indir, EvalOpt,
	Parallel, Locking, NoVector, Profile, NoSubst, Stingy, nuflag, Verbose,
	basename, realname, Code, NoUnrec, Type, NoEvalupdunw, NoZeroFill, Statistics,
	InFlic, NoMOpt, TypeCheck, NoSimpl, NoStrictAnal, NoBConv, NoGOpt,
	NoNoeval, CaseOpt, NoChkInd, SparkKind, NoAsimpl, TestEval, Curry,
	ZapRedex, MemCheckStat, NoElimdup, DoExtra, CopyRet, Unbox, TermCall,
	CopyIndir, NoToprec, TopShow, PrOrignames, FlatNames, NoPrelude, Derived,
	BothTypes, AllowRedef, ExportCheck, RevVis, Pedantic, AutoDerive, Relax, BwmCon,
	StrictCall, FailStrict, RegEntry, DoInline, ExportInline, UsageAnal, AnnotId,
	GenCmp, NewCall, PrErrmap, PrCunrec, PrSubst, PrAppEarley, PrSubEarley, ConcType,
	CheckUnusedVar, GenRead, Strcmp, LinkWord, Interactive, ProfileHeap,
	Prelude, forceprefix, Sparc8, I486, I586, UseRestr, FastConst, PatBindUpdate,
        LocalQuant, OverloadRestr, ExistEscapeCheck, RecBlock, Specialize,
	Power, PowerPC, LocalInst, HigherKind, NewType, EvalClass, BadEvalInst, RelaxEval,
        QualifiedNames, Record, Ordering, MultiParam, MakeSelectors,
        AutoSpecialize, WarnOverload, StackStubbing, H1_3, CCall, NPlusK,
        CAFCall, PIC, InstWithCT, EvalInst, MonadCompr, ShowsType, PatVarError,
	PrGRCG, PrNoGcheck, PrSpillMsg, PrGBblock, PrUnbox, PrStrictinfo,
	PrInput, PrRename, PrConstr, PrRemmatch, PrRemdeep, PrAddfrom,
	PrRemSign, PrRemClass, PrRemLazy, PrGenderiv, PrRemzf, PrClasstrans,
	PrEcnv, PrType, PrApconv, PrEqtrans, PrSimpl, PrOutFlic, PrStrict,
	PrAddclose, PrLambdalift, PrBconv, PrAddarity, PrGcodeUnopt,
	PrGcode, PrMcode, PrSize, PrMtrans, PrAddrestr, PrCurry, PrPredef,
	PrGAny, PrGLive, PrGFlow, PrGContents, PrGDup, PrGSave, PrGTemp,
	PrGJoin, PrG2Op, PrGSpill,
	PrOutBwm, PrTransformed,
        X1, X2, X3, X4, Test, TestN,
	GenStabs, Trace, FloatInstr, UseSpecInst, UseSpecFunc, DoForceArity, UseForceArity, 
        Optimize, EtaExpand, FlatSuper, FastGCCheck, ConstCaf, ConstReturnCaf, InlineCallMet, InlineFun, Optlevel, 
        PreTupleSel, FarJump, Globalize, CaseTagOptim, FastTagOptim, NumOverload, ShowTopTypes,
	SynExpand, Solaris, Linux, CSymbols, fileargs, LoadShareLib;
#define OTestEval true
#define OFastGCCheck true
rec
    args = conc (filter isaflag argv)
and isaflag ('-'.c._) = ~ mem c "pf"
 || isaflag _ = false
and isflag ('-'._) = true
 || isflag _ = false
and fileargs = filter (not o isflag) argv
and basename =
	case fileargs in
	   [] : Yes ""
	|| (name._) : let n = name@".p" in
		    case openfile n in
			Yes s : Yes name
		    ||  No s : No (s@" "@n)
		    end
--	|| _ : No "More than one input file"
	end
and realname =
	case filter (not o isflag) argv in
	   [_;name] : name
	|| _ : "unknown"
        end
and has d n = hasx 'f' d n
and mhas d n = hasx 'm' d n
and phas d n  = hasx 'f' d n | hasx 'p' d n
and hasx x d n   = 
    if mem ('-'.x.n) argv then
	true
    else if mem ('-'.x.'n'.'o'.'-'.n) argv then
	false
    else
	d

and Prelude	  =  has false        "prelude"
and forceprefix   =  if Prelude & ~H1_3 then Some preludename else None
and Curry	  =  has false        "curry"
and Debug	  =  has false        "debug" | IdDebug
and IdDebug       =  has false        "id-debug"
and ImpDebug	  =  has false        "import-debug"
and Strict	  =  has false        "strict"
and Semistrict	  =  has false        "semi-strict"
and Gflag	  =  has false        "out-g-code"
and Fullname	  =  has false        "fullname"
and Indir	  =  has false        "indir"
and EvalOpt	  =  has true         "eval-opt"
and NoNoeval	  = ~has true         "noeval"
and NoAsimpl	  = ~has true         "asimpl"
and CaseOpt	  =  has false        "caseopt"
and Parallel	  =  has false        "parallel"
and Locking	  =  has Parallel     "locking"
and NoVector	  = ~has true         "vector"
and Profile	  =  has false        "profile"
and ProfileHeap	  =  has false        "profile-heap"
and NoSubst	  = ~has true         "subst"
and Stingy	  =  has false        "stingy"
and nuflag	  =  has false        "nu"
and Verbose	  =  has false        "verbose"
and NoMOpt	  = ~has true         "m-opt"
and NoUnrec	  = ~has true         "unrec"
and NoEvalupdunw  = ~has true         "evalupdunw"
and NoChkInd	  = ~has Indir        "chkind"
and Type	  =  has true         "type"
and UseRestr      =  has (~Pedantic)  "use-restr" | H1_3
and FastConst     =  has true         "fast-const" & ~Interactive
and PatBindUpdate =  (has false        "pat-bind-update" | has false "pbu") & ~Interactive
and RecBlock	  =  has true         "rec-block"
and LocalQuant	  =  has (~Pedantic)  "local-quant" | has false  "exist-type"
and ExistEscapeCheck = has true       "exist-escape-check"
and OverloadRestr =  has true         "overload-restr"
and BothTypes	  =  has false        "both"
and AnnotId       =  has false        "annot-id"	| Pedantic
and AllowRedef	  =  has Prelude      "allow-redef"
and ExportCheck	  =  has (~Prelude)   "export-check"
and RevVis        =  has (~Prelude)   "rev-vis"
and Statistics	  =  has false        "statistics"
and ZapRedex	  =  has true         "zap-redex"
and MemCheckStat  =  has false        "memcheckstat"
and TypeCheck	  =  Type | PrType
and NoSimpl	  = ~has true         "simpl"
and NoStrictAnal  = ~has true         "strict-anal"
and NoBConv	  = ~has true         "b-conv"
and NoGOpt	  = ~has true         "g-opt"
and Code	  =  has (~PrOutBwm)  "code"
and InFlic	  =  has false        "in-flic"
and NoZeroFill	  = ~has false        "zero-fill"
and NoElimdup	  = ~has true         "elimdup"
and SparkKind	  =  if has false "spark-call" then 'c' else if ~has true "spark" then 'n' else 'i'
and TestEval	  =  has false        "test-eval" | Optimize & OTestEval
and CopyRet	  =  has true         "copy-ret"
and CopyIndir	  =  has false        "copy-indir"
and FlatNames	  =  has (~Curry)     "flat-names"
and NoPrelude     = ~has true         "use-prelude"
and NoToprec	  = ~has true         "toprec"
and Pedantic	  =  has false        "pedantic"
and AutoDerive    =  has false        "auto-derive"
and Relax         =  has false        "relax"
and GenCmp	  =  has false        "gen-cmp"
and Derived       =  has true         "derived"
and CheckUnusedVar=  has false        "check-unused-var"
and GenRead       =  has true         "gen-read"
and Strcmp        =  has true         "strcmp"
and ConcType	  =  has false        "conctype"
and Unbox         =  has true         "unbox"
and BwmCon	  =  has false        "bwm-con"
and Trace         =  has false        "trace"
and StrictCall    =  has Optimize     "strict-call"
and LinkWord      =  has true         "link-word"
and Interactive   =  has false        "interactive"
and LoadShareLib  =  has false        "load-share-lib"
and TermCall      =  has true         "term-call"
and FailStrict    =  has Optimize     "fail-strict"
and RegEntry      =  has Optimize     "reg-entry"
and DoInline      =  has Optimize     "do-inline"
and InlineFun     =  has Optimize     "inline-fun"
and ExportInline  =  has Optimize     "export-inline"
and UsageAnal     =  has false        "usage-anal"
and Specialize    =  has Curry        "specialize"	| AutoSpecialize
and AutoSpecialize=  has Optimize     "auto-specialize" & Curry
and GenStabs      =  has Trace        "gen-stabs"
and FloatInstr    =  has true         "float-instr"
and UseSpecInst   =  has true         "use-spec-inst"
and UseSpecFunc   =  has true         "use-spec-func"
and DoForceArity  =  has true         "do-force-arity"
and UseForceArity =  has true         "use-force-arity"
and EtaExpand     =  has true         "eta-expand"
and FlatSuper     =  has true         "flat-super"
and FastGCCheck   =  has false        "fast-gc-check"	| Optimize & OFastGCCheck
and ConstCaf      =  has true         "const-caf"
and ConstReturnCaf=  has false        "const-return-caf" & ConstCaf -- do we want CAFs in the R scheme?
and InlineCallMet =  has true	      "inline-call-met"
and PreTupleSel   =  has (~Optimize)  "pre-tuple-sel"
and Globalize     =  has Optimize     "globalize"
and CaseTagOptim  =  has true         "case-tag-optim"
and FastTagOptim  =  has false        "fast-tag-optim"
and NumOverload   =  has true         "Num-overload"
and ShowTopTypes  =  has false        "show-top-types"
and ShowsType     =  has true         "showsType"
and SynExpand	  =  has H1_3	      "syn-expand"
and CSymbols      =  has false        "C-symbols"
and WarnOverload  =  has false        "warn-overload"
and StackStubbing =  has false        "stack-stubbing"
--and H1_3          =  has false        "1.3"
and H1_3          = ~has false        "1.2"		& Curry
and NPlusK        =  has (~H1_3)      "n+k"
and CCall         =  has false        "ccall"
and CAFCall	  =  has true	      "caf-call"
and LocalInst     =  has H1_3         "local-inst"
and InstWithCT    =  has (~H1_3 | Prelude | true) "inst-with-c-t" -- XXX
and MonadCompr    =  has H1_3	      "monad-compr"
and EvalInst      =  has Prelude      "eval-inst"
and HigherKind    =  has H1_3         "higher-kind"
and NewType       =  has H1_3         "new-type"
and EvalClass     =  has H1_3         "eval-class"
and RelaxEval     =  has false        "relax-eval"
and BadEvalInst   =  has true         "bad-eval-inst"
and QualifiedNames=  has H1_3         "qualified-names"
and Record        =  has H1_3         "record"
and MakeSelectors =  has true	      "make-selectors"
and Ordering      =  has H1_3         "ordering"
and MultiParam    =  has false        "multi-param"
and PIC	          =  has false	      "pic"
and PatVarError   =  has false        "pat-var-error"

and Sparc8        =  mhas false       "sparc8"
and FarJump       =  mhas true        "far-jump"
and I486	  =  mhas false	      "i486"
and I586	  =  mhas true	      "i586"
and Solaris       =  mhas false       "solaris"
and Linux         =  mhas false       "linux"

and Power         =  mhas isAIX2_3    "POWER"
and PowerPC       =  mhas false       "PowerPC"

and (Optimize, Optlevel) = case filter (\x.head 2 x = "-O") argv in [] : (false, 0) || (_._.s)._ : (true, max 1 (stoi s)) end

and PrOrignames	  =  phas false       "orignames"
and PrAll	  =  phas false       "all"
and PrErrmap	  =  phas false       "errmap"
and PrInput	  =  phas false       "input"		| PrAll
and PrCurry	  =  phas false       "pcurry"		| PrAll
and PrRename	  =  phas false       "rename"		| PrAll
and PrConstr	  =  phas false       "constr"		| PrAll
and PrRemmatch	  =  phas false       "rem-match"	| PrAll
and PrRemSign	  =  phas false       "rem-sign"	| PrAll
and PrRemClass	  =  phas false       "rem-class"	| PrAll
and PrRemLazy	  =  phas false       "rem-lazy"	| PrAll
and PrGenderiv	  =  phas false       "genderiv"	| PrAll
and PrRemzf	  =  phas false       "remzf"		| PrAll
and PrRemdeep	  =  phas false       "remdeep"		| PrAll
and PrEcnv	  =  phas false       "Ecnv"	        | PrAll
and PrAddrestr	  =  phas false       "addrestr"        | PrAll
and PrAddfrom	  =  phas false       "addfrom"		| PrAll
and PrType	  =  phas false       "ptype"		| PrAll
and PrApconv	  =  phas false       "apconv"		| PrAll
and PrEqtrans	  =  phas false       "eqtrans"		| PrAll
and PrClasstrans  =  phas false       "classtrans"	| PrAll
and PrPredef	  =  phas false       "predef"		| PrAll
and PrSimpl	  =  phas false       "simpl"		| PrAll
and PrOutFlic	  =  phas false       "out-flic"
and PrOutBwm	  =  phas false       "out-bwm" | has false        "bwm"
and PrStrict	  =  phas false       "pstrict"	        | PrAll
and PrAddclose	  =  phas false       "addclose"	| PrAll
and PrLambdalift  =  phas false       "lambdalift"	| PrAll
and PrBconv	  =  phas false       "bconv"		| PrAll
and PrAddarity	  =  phas false       "addarity"	| PrAll
and PrGcodeUnopt  =  phas false       "gcode-unopt"	| PrAll
and PrGcode	  =  phas false       "gcode"		| PrAll
and PrMcode	  =  phas false       "mcode"		| PrAll
and PrSize	  =  phas false       "size"	        | PrAll
and PrMtrans	  =  phas false       "mtrans"
and PrUnbox	  =  phas false       "unbox"		| PrAll
and PrStrictinfo  =  phas false       "strictinfo"      | PrAll
and PrTransformed =  phas false       "transformed"

and PrCunrec      =  phas false       "cunrec" & ConcType      | PrAll
and PrAppEarley   =  phas false       "appearley" & ConcType   | PrAll
and PrSubEarley   =  phas false       "subearley" & ConcType   | PrAll
and PrSubst       =  phas false       "subst" & ConcType       | PrAll

and TopShow	  =  phas false       "top-show"		-- Only for interactive

and PrGAll        =  phas false       "g-all"
and PrGLive	  =  phas false       "g-live"       | PrGAll
and PrGFlow	  =  phas false       "g-flow"       | PrGAll
and PrGContents	  =  phas false       "g-contents"   | PrGAll
and PrGDup	  =  phas false       "g-dup"        | PrGAll
and PrGSave	  =  phas false       "g-save"       | PrGAll
and PrGTemp	  =  phas false       "g-temp"       | PrGAll
and PrGJoin	  =  phas false       "g-join"       | PrGAll
and PrG2Op	  =  phas false       "g-2-op"       | PrGAll
and PrGRCG	  =  phas false       "g-rcg"        | PrGAll
and PrGSpill	  =  phas false       "g-spill"      | PrGAll
and PrGBblock	  =  phas false       "g-bblock"     | PrGAll
and PrNoGcheck	  = ~phas true        "gcheck"
and PrSpillMsg	  =  phas false       "spill-msg"    | PrGAll
and PrGAny	  =  PrGLive|PrGFlow|PrGContents|PrGDup|PrGSave|PrGTemp|PrGJoin
and DoExtra	  =  (f argv
		   where rec f []   =  1
		       ||    f (x.xs)   =  if head 10 x   =    "-fdo-extra" then
			                    stoi (tail 10 x)
					else
					    f xs)
and Test          =  has false        "test"
and NewCall       =  has false        "new-call"
and TestN         =  length (filter (\a.a="-ftest") argv)

-- Flags for temporary use
and X1		  =  has false   "X1"
and X2		  =  has false   "X2"
and X3		  =  has false   "X3"
and X4		  =  has false   "X4"

#ifdef _AIX
and isAIX2_3 = let (_,_,rel,ver,_) = uname in rel="2" & ver="3"
#else
and isAIX2_3 = false
#endif

end
