module _LibIOErrno(isAlreadyExistsError, isAlreadyInUseError, isFullError, isIllegalOperation, isPermissionError, isDoesNotExistError) where

import _LibDialogue
import _LibIO

#include "myerrno.h"

alreadyExists, alreadyInUse, full, illegalOperation, permission, notFound :: [Int]
alreadyExists = [eEXIST]
alreadyInUse = [eBUSY, eTXTBSY, eALREADY, eADDRINUSE]
full = [e2BIG, eNOMEM, eNFILE, eMFILE, eNOSPC, eAGAIN, eMSGSIZE, eNOBUFS]
illegalOperation = [eNOTBLK, eNODEV, eISDIR, eINVAL, eNOTTY, eSPIPE, eDOM, eRANGE, eNOTSOCK, ePROTOTYPE]
permission = [ePERM, eNOEXEC, eBADF, eACCES, eFBIG]
notFound = [eNOENT, eSRCH, eNETDOWN, eNETUNREACH]

isAlreadyExistsError (IOError (PosixErrno e _) _ _) = e `elem` alreadyExists

isAlreadyInUseError (IOError (PosixErrno e _) _ _) = e `elem` alreadyInUse

isFullError (IOError (PosixErrno e _) _ _) = e `elem` full

isIllegalOperation (IOError (PosixErrno e _) _ _) = e `elem` illegalOperation

isPermissionError (IOError (PosixErrno e _) _ _) = e `elem` permission

isDoesNotExistError (IOError (PosixErrno e _) _ _) = e `elem` notFound

