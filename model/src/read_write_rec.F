C $Header: /u/gcmpack/MITgcm/model/src/Attic/read_write_rec.F,v 1.5 2001/09/26 18:09:16 cnh Exp $
C $Name:  $
#include "CPP_OPTIONS.h"

C--  File read_write_rec.F: Routines to handle mid-level I/O interface.
C--   Contents
C--   o SET_WRITE_GLOBAL_REC
C--   o READ_REC_XY_RS
C--   o READ_REC_XY_RL
C--   o READ_REC_XYZ_RS
C--   o READ_REC_XYZ_RL
C--   o READ_REC_XZ_RS
C--   o READ_REC_XZ_RL
C--   o READ_REC_YZ_RS
C--   o READ_REC_YZ_RL
C--   o WRITE_REC_XY_RS
C--   o WRITE_REC_XY_RL
C--   o WRITE_REC_XYZ_RS
C--   o WRITE_REC_XYZ_RL

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
C
      globalFile=flag
C
      RETURN
      END

CBOP
C     !ROUTINE: READ_REC_XY_RS
C     !INTERFACE:
      SUBROUTINE READ_REC_XY_RS( fName,field,iRec,myIter,myThid)
C     !DESCRIPTION: \bv
C READ_REC_XY_RS is a "front-end" interface to the low-level I/O
C routines. It assumes single record files.
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
C     INTEGER ILNBLNK
C     !LOCAL VARIABLES:
C Local
      CHARACTER*(2) fType
      INTEGER nNz
C     INTEGER IL
      CHARACTER*(80) fullName
CEOP
C
C     IF (myIter.GE.0) THEN
C      IL=ILNBLNK( fName )
C      WRITE(fullName(1:80),'(2a,i10.10)') fName(1:IL),'.',myIter
C     ELSE
       fullName=fName
C     ENDIF
C
      fType='RS'
      nNz=1
      CALL MDSREADFIELD( fullName, readBinaryPrec, fType,
     &     nNz, field, irec, myThid )
C
      RETURN
      END

CBOP
C     !ROUTINE: READ_REC_XY_RL
C     !INTERFACE:
      SUBROUTINE READ_REC_XY_RL( fName,field,iRec,myIter,myThid)

C     !DESCRIPTION: \bv
C READ_REC_XY_RL is a "front-end" interface to the low-level I/O
C routines. It assumes single record files.
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
C     INTEGER ILNBLNK

C     !LOCAL VARIABLES:
C Local
      CHARACTER*(2) fType
      INTEGER nNz
C     INTEGER IL
      CHARACTER*(80) fullName
CEOP
C
C     IF (myIter.GE.0) THEN
C      IL=ILNBLNK( fName )
C      WRITE(fullName(1:80),'(2a,i10.10)') fName(1:IL),'.',myIter
C     ELSE
       fullName=fName
C     ENDIF
C
      fType='RL'
      nNz=1
      CALL MDSREADFIELD( fullName, readBinaryPrec, fType,
     &     nNz, field, irec, myThid )
C
      RETURN
      END

CBOP
C     !ROUTINE: READ_REC_XYZ_RS
C     !INTERFACE:
      SUBROUTINE READ_REC_XYZ_RS( fName,field,iRec,myIter,myThid)

C     !DESCRIPTION: \bv
C READ_REC_XYZ_RS is a "front-end" interface to the low-level I/O
C routines. It assumes single record files.
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
C     INTEGER ILNBLNK

C     !LOCAL VARIABLES:
C Local
      CHARACTER*(2) fType
      INTEGER nNz
C     INTEGER IL
      CHARACTER*(80) fullName
CEOP

C
C     IF (myIter.GE.0) THEN
C      IL=ILNBLNK( fName )
C      WRITE(fullName(1:80),'(2a,i10.10)') fName(1:IL),'.',myIter
C     ELSE
       fullName=fName
C     ENDIF
C
      fType='RS'
      nNz=Nr
      CALL MDSREADFIELD( fullName, readBinaryPrec, fType,
     &     nNz, field, irec, myThid )
C
      RETURN
      END

CBOP
C     !ROUTINE: READ_REC_XYZ_RL
C     !INTERFACE:
      SUBROUTINE READ_REC_XYZ_RL( fName,field,iRec,myIter,myThid)

C     !DESCRIPTION: \bv
C READ_REC_XYZ_RL is a "front-end" interface to the low-level I/O
C routines. It assumes single record files.
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
C     INTEGER ILNBLNK

C     !LOCAL VARIABLES:
C Local
      CHARACTER*(2) fType
      INTEGER nNz
C     INTEGER IL
      CHARACTER*(80) fullName
CEOP

C
C     IF (myIter.GE.0) THEN
C      IL=ILNBLNK( fName )
C      WRITE(fullName(1:80),'(2a,i10.10)') fName(1:IL),'.',myIter
C     ELSE
       fullName=fName
C     ENDIF
C
      fType='RL'
      nNz=Nr
      CALL MDSREADFIELD( fullName, readBinaryPrec, fType,
     &     nNz, field, irec, myThid )
C
      RETURN
      END

CBOP
C     !ROUTINE: READ_REC_XZ_RS
C     !INTERFACE:
      SUBROUTINE READ_REC_XZ_RS( fName,field,iRec,myIter,myThid)

C     !DESCRIPTION: \bv
C READ_REC_XZ_RS is a "front-end" interface to the low-level I/O
C routines. It assumes single record files.
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
      _RS field(1-Olx:sNx+Olx,Nr,nSx,nSy)
      INTEGER iRec
      INTEGER myIter
      INTEGER myThid
C Functions
C     INTEGER ILNBLNK

C     !LOCAL VARIABLES:
C Local
      CHARACTER*(2) fType
      INTEGER nNz
C     INTEGER IL
      CHARACTER*(80) fullName
CEOP
C
C     IF (myIter.GE.0) THEN
C      IL=ILNBLNK( fName )
C      WRITE(fullName(1:80),'(2a,i10.10)') fName(1:IL),'.',myIter
C     ELSE
       fullName=fName
C     ENDIF
C
      fType='RS'
      nNz=Nr
      CALL MDSREADFIELDXZ( fullName, readBinaryPrec, fType,
     &     nNz, field, irec, myThid )
C
      RETURN
      END

CBOP
C     !ROUTINE: READ_REC_XZ_RL
C     !INTERFACE:
      SUBROUTINE READ_REC_XZ_RL( fName,field,iRec,myIter,myThid)

C     !DESCRIPTION: \bv
C READ_REC_XZ_RL is a "front-end" interface to the low-level I/O
C routines. It assumes single record files.
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
      _RL field(1-Olx:sNx+Olx,Nr,nSx,nSy)
      INTEGER iRec
      INTEGER myIter
      INTEGER myThid
C Functions
C     INTEGER ILNBLNK

C     !LOCAL VARIABLES:
C Local
      CHARACTER*(2) fType
      INTEGER nNz
C     INTEGER IL
      CHARACTER*(80) fullName
CEOP
C
C     IF (myIter.GE.0) THEN
C      IL=ILNBLNK( fName )
C      WRITE(fullName(1:80),'(2a,i10.10)') fName(1:IL),'.',myIter
C     ELSE
       fullName=fName
C     ENDIF
C
      fType='RL'
      nNz=Nr
      CALL MDSREADFIELDXZ( fullName, readBinaryPrec, fType,
     &     nNz, field, irec, myThid )
C
      RETURN
      END

CBOP
C     !ROUTINE: READ_REC_YZ_RS
C     !INTERFACE:
      SUBROUTINE READ_REC_YZ_RS( fName,field,iRec,myIter,myThid)

C     !DESCRIPTION: \bv
C READ_REC_YZ_RS is a "front-end" interface to the low-level I/O
C routines. It assumes single record files.
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
      _RS field(1-Oly:sNy+Oly,Nr,nSx,nSy)
      INTEGER iRec
      INTEGER myIter
      INTEGER myThid
C Functions
C     INTEGER ILNBLNK

C     !LOCAL VARIABLES:
C Local
      CHARACTER*(2) fType
      INTEGER nNz
C     INTEGER IL
      CHARACTER*(80) fullName
CEOP

C
C     IF (myIter.GE.0) THEN
C      IL=ILNBLNK( fName )
C      WRITE(fullName(1:80),'(2a,i10.10)') fName(1:IL),'.',myIter
C     ELSE
       fullName=fName
C     ENDIF
C
      fType='RS'
      nNz=Nr
      CALL MDSREADFIELDYZ( fullName, readBinaryPrec, fType,
     &     nNz, field, irec, myThid )
C
      RETURN
      END

CBOP
C     !ROUTINE: READ_REC_YZ_RL
C     !INTERFACE:
      SUBROUTINE READ_REC_YZ_RL( fName,field,iRec,myIter,myThid)

C     !DESCRIPTION: \bv
C READ_REC_YZ_RL is a "front-end" interface to the low-level I/O
C routines. It assumes single record files.
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
      _RL field(1-Oly:sNy+Oly,Nr,nSx,nSy)
      INTEGER iRec
      INTEGER myIter
      INTEGER myThid
C Functions
C     INTEGER ILNBLNK

C     !LOCAL VARIABLES:
C Local
      CHARACTER*(2) fType
      INTEGER nNz
C     INTEGER IL
      CHARACTER*(80) fullName
CEOP

C
C     IF (myIter.GE.0) THEN
C      IL=ILNBLNK( fName )
C      WRITE(fullName(1:80),'(2a,i10.10)') fName(1:IL),'.',myIter
C     ELSE
       fullName=fName
C     ENDIF
C
      fType='RL'
      nNz=Nr
      CALL MDSREADFIELDYZ( fullName, readBinaryPrec, fType,
     &     nNz, field, irec, myThid )
C
      RETURN
      END

CBOP
C     !ROUTINE: WRITE_REC_XY_RS
C     !INTERFACE:
      SUBROUTINE WRITE_REC_XY_RS( fName,field,iRec,myIter,myThid)

C     !DESCRIPTION: \bv
C WRITE_REC_XY_RS is a "front-end" interface to the low-level I/O
C routines. It assumes single record files.
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
C     INTEGER ILNBLNK
C Common
      COMMON /RD_WR_REC/ globalFile
      LOGICAL globalFile

C     !LOCAL VARIABLES:
C Local
      CHARACTER*(2) fType
      INTEGER nNz
C     INTEGER IL
      CHARACTER*(80) fullName
CEOP

C
C     IF (myIter.GE.0) THEN
C      IL=ILNBLNK( fName )
C      WRITE(fullName(1:80),'(2a,i10.10)') fName(1:IL),'.',myIter
C     ELSE
       fullName=fName
C     ENDIF
C
      fType='RS'
      nNz=1
      globalFile=.FALSE.
      CALL MDSWRITEFIELD( fullName, writeBinaryPrec, globalFile,
     &     fType, nNz, field, irec, myIter, myThid )
C
      RETURN
      END

CBOP
C     !ROUTINE: WRITE_REC_XY_RL
C     !INTERFACE:
      SUBROUTINE WRITE_REC_XY_RL( fName,field,iRec,myIter,myThid)

C     !DESCRIPTION: \bv
C WRITE_REC_XY_RL is a "front-end" interface to the low-level I/O
C routines. It assumes single record files.
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
C     INTEGER ILNBLNK
C Common
      COMMON /RD_WR_REC/ globalFile
      LOGICAL globalFile

C     !LOCAL VARIABLES:
C Local
      CHARACTER*(2) fType
      INTEGER nNz
C     INTEGER IL
      CHARACTER*(80) fullName
CEOP

C
C     IF (myIter.GE.0) THEN
C      IL=ILNBLNK( fName )
C      WRITE(fullName(1:80),'(2a,i10.10)') fName(1:IL),'.',myIter
C     ELSE
       fullName=fName
C     ENDIF
C
      fType='RL'
      nNz=1
      globalFile=.FALSE.
      CALL MDSWRITEFIELD( fullName, writeBinaryPrec, globalFile,
     &     fType, nNz, field, irec, myIter, myThid )
C
      RETURN
      END

CBOP
C     !ROUTINE: WRITE_REC_XYZ_RS
C     !INTERFACE:
      SUBROUTINE WRITE_REC_XYZ_RS( fName,field,iRec,myIter,myThid)

C     !DESCRIPTION: \bv
C WRITE_REC_XYZ_RS is a "front-end" interface to the low-level I/O
C routines. It assumes single record files.
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
C     INTEGER ILNBLNK
C Common
      COMMON /RD_WR_REC/ globalFile
      LOGICAL globalFile

C     !LOCAL VARIABLES:
C Local
      CHARACTER*(2) fType
      INTEGER nNz
C     INTEGER IL
      CHARACTER*(80) fullName
CEOP

C
C     IF (myIter.GE.0) THEN
C      IL=ILNBLNK( fName )
C      WRITE(fullName(1:80),'(2a,i10.10)') fName(1:IL),'.',myIter
C     ELSE
       fullName=fName
C     ENDIF
C
      fType='RS'
      nNz=Nr
      globalFile=.FALSE.
      CALL MDSWRITEFIELD( fullName, writeBinaryPrec, globalFile,
     &     fType, nNz, field, irec, myIter, myThid )
C
      RETURN
      END

CBOP
C     !ROUTINE: WRITE_REC_XYZ_RL
C     !INTERFACE:
      SUBROUTINE WRITE_REC_XYZ_RL( fName,field,iRec,myIter,myThid)

C     !DESCRIPTION: \bv
C WRITE_REC_XYZ_RL is a "front-end" interface to the low-level I/O
C routines. It assumes single record files.
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
C     INTEGER ILNBLNK
C Common
      COMMON /RD_WR_REC/ globalFile
      LOGICAL globalFile

C     !LOCAL VARIABLES:
C Local
      CHARACTER*(2) fType
      INTEGER nNz
C     INTEGER IL
      CHARACTER*(80) fullName
CEOP

C
C     IF (myIter.GE.0) THEN
C      IL=ILNBLNK( fName )
C      WRITE(fullName(1:80),'(2a,i10.10)') fName(1:IL),'.',myIter
C     ELSE
       fullName=fName
C     ENDIF
C
      fType='RL'
      nNz=Nr
      globalFile=.FALSE.
      CALL MDSWRITEFIELD( fullName, writeBinaryPrec, globalFile,
     &     fType, nNz, field, irec, myIter, myThid )
C
      RETURN
      END
