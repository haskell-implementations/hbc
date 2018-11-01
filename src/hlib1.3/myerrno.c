#include <errno.h>
eEXIST = EEXIST :: Int
eBUSY = EBUSY :: Int
eTXTBSY = ETXTBSY :: Int
#ifdef EALREADY
eALREADY = EALREADY :: Int
#else
eALREADY = -1 :: Int
#endif
#ifdef EADDRINUSE
eADDRINUSE = EADDRINUSE :: Int
#else
eADDRINUSE = -1 :: Int
#endif
e2BIG = E2BIG :: Int
eNOMEM = ENOMEM :: Int
eNFILE = ENFILE :: Int
eMFILE = EMFILE :: Int
eNOSPC = ENOSPC :: Int
eAGAIN = EAGAIN :: Int
#ifdef EMSGSIZE
eMSGSIZE = EMSGSIZE :: Int
#else
eMSGSIZE = -1 :: Int
#endif
eNOBUFS = ENOBUFS :: Int
eNOTBLK = ENOTBLK :: Int
eNODEV = ENODEV :: Int
eISDIR = EISDIR :: Int
eINVAL = EINVAL :: Int
eNOTTY = ENOTTY :: Int
eSPIPE = ESPIPE :: Int
eDOM = EDOM :: Int
eRANGE = ERANGE :: Int
eNOTSOCK = ENOTSOCK :: Int
ePROTOTYPE = EPROTOTYPE :: Int
ePERM = EPERM :: Int
eNOEXEC = ENOEXEC :: Int
eBADF = EBADF :: Int
eACCES = EACCES :: Int
eFBIG = EFBIG :: Int
eNOENT = ENOENT :: Int
eSRCH = ESRCH :: Int
#ifdef ENETDOWN
eNETDOWN = ENETDOWN :: Int
#else
eNETDOWN = -1 :: Int
#endif
#ifdef ENETUNREACH
eNETUNREACH = ENETUNREACH :: Int
#else
eNETUNREACH = -1 :: Int
#endif
