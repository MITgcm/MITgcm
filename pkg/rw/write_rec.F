C $Header: /u/gcmpack/MITgcm/pkg/rw/write_rec.F,v 1.1 2006/08/06 18:06:17 jmc Exp $
C $Name:  $
#include "RW_OPTIONS.h"

C--  File read_write_rec.F: Routines to handle mid-level I/O interface.
C--   Contents
C--   o SET_WRITE_GLOBAL_REC
C--   o WRITE_REC_XY_RS
C--   o WRITE_REC_XY_RL
C--   o WRITE_REC_XYZ_RS
C--   o WRITE_REC_XYZ_RL
C--   o WRITE_REC_3D_RS
C--   o WRITE_REC_3D_RL

CBOP
C     !ROUTINE: SET_WRITE_GLOBAL_REC
C     !INTERFACE:
      SUBROUTINE SET_WRITE_GLOBAL_REC ( flag )
      IMPLICIT NONE

C     !DESCRIPTION: \bv
C SET_WRITE_GLOBAL_FLD( flag ) sets an internal logical state to
C indicate whether files written by subsequent call to the
C READ_WRITE_FLD package should create "global" or "tiled" files.
C   flag = .TRUE.  indicates "global" files
C   flag = .FALSE. indicates "tiled" files
C     \ev

C     !INPUT/OUTPUT PARAMETERS:
C Arguments
      LOGICAL flag
C     !USES:
C Common
      COMMON /RD_WR_REC/ globalFile
      LOGICAL globalFile
CEOP

      globalFile=flag

      RETURN
      END

CBOP
C     !ROUTINE: WRITE_REC_XY_RS
C     !INTERFACE:
      SUBROUTINE WRITE_REC_XY_RS( fName,field,iRec,myIter,myThid)

C     !DESCRIPTION: \bv
C WRITE_REC_XY_RS is a "front-end" interface to the low-level I/O
C routines.
C     \ev

C     !USES:
      IMPLICIT NONE
C Global
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C Arguments
      CHARACTER*(*) fName
      _RS field(1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      INTEGER iRec
      INTEGER myIter
      INTEGER myThid

C Functions
c     INTEGER ILNBLNK

C Common
      COMMON /RD_WR_REC/ globalFile
      LOGICAL globalFile

C     !LOCAL VARIABLES:
C Local
      CHARACTER*(2) fType
      INTEGER nNz
c     INTEGER IL
c     CHARACTER*(MAX_LEN_FNAM) fullName
CEOP


c     IF (myIter.GE.0) THEN
c      IL=ILNBLNK( fName )
c      WRITE(fullName,'(2a,i10.10)') fName(1:IL),'.',myIter
c     ELSE
c      fullName=fName
c     ENDIF

      fType='RS'
      nNz=1
c     globalFile=.FALSE.
#ifdef ALLOW_MDSIO
      CALL MDSWRITEFIELD( fName, writeBinaryPrec, globalFile,
     &     fType, nNz, field, iRec, myIter, myThid )
#endif
      RETURN
      END

CBOP
C     !ROUTINE: WRITE_REC_XY_RL
C     !INTERFACE:
      SUBROUTINE WRITE_REC_XY_RL( fName,field,iRec,myIter,myThid)

C     !DESCRIPTION: \bv
C WRITE_REC_XY_RL is a "front-end" interface to the low-level I/O
C routines.
C     \ev

C     !USES:
      IMPLICIT NONE
C Global
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C Arguments
      CHARACTER*(*) fName
      _RL field(1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      INTEGER iRec
      INTEGER myIter
      INTEGER myThid

C Functions
c     INTEGER ILNBLNK

C Common
      COMMON /RD_WR_REC/ globalFile
      LOGICAL globalFile

C     !LOCAL VARIABLES:
C Local
      CHARACTER*(2) fType
      INTEGER nNz
c     INTEGER IL
c     CHARACTER*(MAX_LEN_FNAM) fullName
CEOP


c     IF (myIter.GE.0) THEN
c      IL=ILNBLNK( fName )
c      WRITE(fullName,'(2a,i10.10)') fName(1:IL),'.',myIter
c     ELSE
c      fullName=fName
c     ENDIF

      fType='RL'
      nNz=1
c     globalFile=.FALSE.
#ifdef ALLOW_MDSIO
      CALL MDSWRITEFIELD( fName, writeBinaryPrec, globalFile,
     &     fType, nNz, field, iRec, myIter, myThid )
#endif
      RETURN
      END

CBOP
C     !ROUTINE: WRITE_REC_XYZ_RS
C     !INTERFACE:
      SUBROUTINE WRITE_REC_XYZ_RS( fName,field,iRec,myIter,myThid)

C     !DESCRIPTION: \bv
C WRITE_REC_XYZ_RS is a "front-end" interface to the low-level I/O
C routines.
C     \ev

C     !USES:
      IMPLICIT NONE
C Global
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C Arguments
      CHARACTER*(*) fName
      _RS field(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      INTEGER iRec
      INTEGER myIter
      INTEGER myThid

C Functions
c     INTEGER ILNBLNK

C Common
      COMMON /RD_WR_REC/ globalFile
      LOGICAL globalFile

C     !LOCAL VARIABLES:
C Local
      CHARACTER*(2) fType
      INTEGER nNz
c     INTEGER IL
c     CHARACTER*(MAX_LEN_FNAM) fullName
CEOP


c     IF (myIter.GE.0) THEN
c      IL=ILNBLNK( fName )
c      WRITE(fullName,'(2a,i10.10)') fName(1:IL),'.',myIter
c     ELSE
c      fullName=fName
c     ENDIF

      fType='RS'
      nNz=Nr
c     globalFile=.FALSE.
#ifdef ALLOW_MDSIO
      CALL MDSWRITEFIELD( fName, writeBinaryPrec, globalFile,
     &     fType, nNz, field, iRec, myIter, myThid )
#endif
      RETURN
      END

CBOP
C     !ROUTINE: WRITE_REC_XYZ_RL
C     !INTERFACE:
      SUBROUTINE WRITE_REC_XYZ_RL( fName,field,iRec,myIter,myThid)

C     !DESCRIPTION: \bv
C WRITE_REC_XYZ_RL is a "front-end" interface to the low-level I/O
C routines.
C     \ev

C     !USES:
      IMPLICIT NONE
C Global
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C Arguments
      CHARACTER*(*) fName
      _RL field(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      INTEGER iRec
      INTEGER myIter
      INTEGER myThid

C Functions
c     INTEGER ILNBLNK

C Common
      COMMON /RD_WR_REC/ globalFile
      LOGICAL globalFile

C     !LOCAL VARIABLES:
C Local
      CHARACTER*(2) fType
      INTEGER nNz
c     INTEGER IL
c     CHARACTER*(MAX_LEN_FNAM) fullName
CEOP


c     IF (myIter.GE.0) THEN
c      IL=ILNBLNK( fName )
c      WRITE(fullName,'(2a,i10.10)') fName(1:IL),'.',myIter
c     ELSE
c      fullName=fName
c     ENDIF

      fType='RL'
      nNz=Nr
c     globalFile=.FALSE.
#ifdef ALLOW_MDSIO
      CALL MDSWRITEFIELD( fName, writeBinaryPrec, globalFile,
     &     fType, nNz, field, iRec, myIter, myThid )
#endif
      RETURN
      END

CBOP
C     !ROUTINE: WRITE_REC_3D_RS
C     !INTERFACE:
      SUBROUTINE WRITE_REC_3D_RS(
     I                            fName, fPrec, nNz, field,
     I                            iRec, myIter, myThid )

C     !DESCRIPTION: \bv
C WRITE_REC_3D_RS is a "front-end" interface to the low-level I/O routines.
C  write a full 3D field (nNz levels) to record number "iRec"
C   in binary file "fName" (precision "fPrec", generally =32 or 64).
C     \ev

C     !USES:
      IMPLICIT NONE
C Global
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C Arguments
      CHARACTER*(*) fName
      INTEGER fPrec
      INTEGER nNz
      _RS field(1-Olx:sNx+Olx,1-Oly:sNy+Oly,nNz,nSx,nSy)
      INTEGER iRec
      INTEGER myIter
      INTEGER myThid

C Functions
c     INTEGER ILNBLNK

C Common
      COMMON /RD_WR_REC/ globalFile
      LOGICAL globalFile

C     !LOCAL VARIABLES:
C Local
      CHARACTER*(2) fType
c     INTEGER IL
c     CHARACTER*(MAX_LEN_FNAM) fullName
CEOP


c     IF (myIter.GE.0) THEN
c      IL=ILNBLNK( fName )
c      WRITE(fullName,'(2a,i10.10)') fName(1:IL),'.',myIter
c     ELSE
c      fullName=fName
c     ENDIF

      fType='RS'
c     globalFile=.FALSE.
#ifdef ALLOW_MDSIO
      CALL MDSWRITEFIELD( fName, fPrec, globalFile,
     &     fType, nNz, field, iRec, myIter, myThid )
#endif
      RETURN
      END

CBOP
C     !ROUTINE: WRITE_REC_3D_RL
C     !INTERFACE:
      SUBROUTINE WRITE_REC_3D_RL(
     I                            fName, fPrec, nNz, field,
     I                            iRec, myIter, myThid )

C     !DESCRIPTION: \bv
C WRITE_REC_3D_RL is a "front-end" interface to the low-level I/O routines.
C  write a full 3D field (nNz levels) to record number "iRec"
C   in binary file "fName" (precision "fPrec", generally =32 or 64).
C     \ev

C     !USES:
      IMPLICIT NONE
C Global
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C Arguments
      CHARACTER*(*) fName
      INTEGER fPrec
      INTEGER nNz
      _RL field(1-Olx:sNx+Olx,1-Oly:sNy+Oly,nNz,nSx,nSy)
      INTEGER iRec
      INTEGER myIter
      INTEGER myThid

C Functions
c     INTEGER ILNBLNK

C Common
      COMMON /RD_WR_REC/ globalFile
      LOGICAL globalFile

C     !LOCAL VARIABLES:
C Local
      CHARACTER*(2) fType
c     INTEGER IL
c     CHARACTER*(MAX_LEN_FNAM) fullName
CEOP


c     IF (myIter.GE.0) THEN
c      IL=ILNBLNK( fName )
c      WRITE(fullName,'(2a,i10.10)') fName(1:IL),'.',myIter
c     ELSE
c      fullName=fName
c     ENDIF

      fType='RL'
c     globalFile=.FALSE.
#ifdef ALLOW_MDSIO
      CALL MDSWRITEFIELD( fName, fPrec, globalFile,
     &     fType, nNz, field, iRec, myIter, myThid )
#endif
      RETURN
      END
