C $Header: /u/gcmpack/MITgcm/pkg/rw/read_rec.F,v 1.3 2009/09/01 19:25:28 jmc Exp $
C $Name:  $
#include "RW_OPTIONS.h"

C--  File read_write_rec.F: Routines to handle mid-level I/O interface.
C--   Contents
C--   o READ_REC_XY_RS
C--   o READ_REC_XY_RL
C--   o READ_REC_XYZ_RS
C--   o READ_REC_XYZ_RL
C--   o READ_REC_3D_RS
C--   o READ_REC_3D_RL
C--   o READ_REC_LEV_RS
C--   o READ_REC_LEV_RL
C--   o READ_REC_XZ_RS
C--   o READ_REC_XZ_RL
C--   o READ_REC_YZ_RS
C--   o READ_REC_YZ_RL

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: READ_REC_XY_RS
C     !INTERFACE:
      SUBROUTINE READ_REC_XY_RS( fName,field,iRec,myIter,myThid )
C     !DESCRIPTION: \bv
C READ_REC_XY_RS is a "front-end" interface to the low-level I/O
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

C     !LOCAL VARIABLES:
C Local
      LOGICAL useCurrentDir
      _RL dummyRL(1)
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

      useCurrentDir = .FALSE.
      fType='RS'
      nNz=1
#ifdef ALLOW_MDSIO
      CALL MDS_READ_FIELD(
     I                     fName, readBinaryPrec, useCurrentDir,
     I                     fType, nNz, 1, nNz,
     O                     dummyRL, field,
     I                     iRec, myThid )
#endif
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: READ_REC_XY_RL
C     !INTERFACE:
      SUBROUTINE READ_REC_XY_RL( fName,field,iRec,myIter,myThid )

C     !DESCRIPTION: \bv
C READ_REC_XY_RL is a "front-end" interface to the low-level I/O
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

C     !LOCAL VARIABLES:
C Local
      LOGICAL useCurrentDir
      _RS dummyRS(1)
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

      useCurrentDir = .FALSE.
      fType='RL'
      nNz=1
#ifdef ALLOW_MDSIO
      CALL MDS_READ_FIELD(
     I                     fName, readBinaryPrec, useCurrentDir,
     I                     fType, nNz, 1, nNz,
     O                     field, dummyRS,
     I                     iRec, myThid )
#endif
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: READ_REC_XYZ_RS
C     !INTERFACE:
      SUBROUTINE READ_REC_XYZ_RS( fName,field,iRec,myIter,myThid )

C     !DESCRIPTION: \bv
C READ_REC_XYZ_RS is a "front-end" interface to the low-level I/O
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

C     !LOCAL VARIABLES:
C Local
      LOGICAL useCurrentDir
      _RL dummyRL(1)
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

      useCurrentDir = .FALSE.
      fType='RS'
      nNz=Nr
#ifdef ALLOW_MDSIO
      CALL MDS_READ_FIELD(
     I                     fName, readBinaryPrec, useCurrentDir,
     I                     fType, nNz, 1, nNz,
     O                     dummyRL, field,
     I                     iRec, myThid )
#endif
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: READ_REC_XYZ_RL
C     !INTERFACE:
      SUBROUTINE READ_REC_XYZ_RL( fName,field,iRec,myIter,myThid )

C     !DESCRIPTION: \bv
C READ_REC_XYZ_RL is a "front-end" interface to the low-level I/O
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

C     !LOCAL VARIABLES:
C Local
      LOGICAL useCurrentDir
      _RS dummyRS(1)
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

      useCurrentDir = .FALSE.
      fType='RL'
      nNz=Nr
#ifdef ALLOW_MDSIO
      CALL MDS_READ_FIELD(
     I                     fName, readBinaryPrec, useCurrentDir,
     I                     fType, nNz, 1, nNz,
     O                     field, dummyRS,
     I                     iRec, myThid )
#endif
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: READ_REC_3D_RS
C     !INTERFACE:
      SUBROUTINE READ_REC_3D_RS(
     I                           fName, fPrec, nNz,
     O                           field,
     I                           iRec, myIter, myThid )

C     !DESCRIPTION: \bv
C READ_REC_3D_RS is a "front-end" interface to the low-level I/O routines.
C  read a full 3D field (nNz levels) from record number "iRec"
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

C     !LOCAL VARIABLES:
C Local
      LOGICAL useCurrentDir
      _RL dummyRL(1)
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

      useCurrentDir = .FALSE.
      fType='RS'
#ifdef ALLOW_MDSIO
      CALL MDS_READ_FIELD(
     I                     fName, fPrec, useCurrentDir,
     I                     fType, nNz, 1, nNz,
     O                     dummyRL, field,
     I                     iRec, myThid )
#endif
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: READ_REC_3D_RL
C     !INTERFACE:
      SUBROUTINE READ_REC_3D_RL(
     I                           fName, fPrec, nNz,
     O                           field,
     I                           iRec, myIter, myThid )

C     !DESCRIPTION: \bv
C READ_REC_3D_RL is a "front-end" interface to the low-level I/O routines.
C  read a full 3D field (nNz levels) from record number "iRec"
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

C     !LOCAL VARIABLES:
C Local
      LOGICAL useCurrentDir
      _RS dummyRS(1)
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

      useCurrentDir = .FALSE.
      fType='RL'
#ifdef ALLOW_MDSIO
      CALL MDS_READ_FIELD(
     I                     fName, fPrec, useCurrentDir,
     I                     fType, nNz, 1, nNz,
     O                     field, dummyRS,
     I                     iRec, myThid )
#endif
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: READ_REC_LEV_RS
C     !INTERFACE:
      SUBROUTINE READ_REC_LEV_RS(
     I                           fName, fPrec, kSiz, kLo, kHi,
     O                           field,
     I                           iRec, myIter, myThid )

C     !DESCRIPTION: \bv
C READ_REC_LEV_RS is a "front-end" interface to the low-level I/O routines.
C  Read a set a consecutive levels (from kLo to kHi) into 3D array (size: kSiz)
C  from record number "iRec" in binary file "fName" (precision "fPrec",
C  generally =32 or 64).
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
      INTEGER kSiz, kLo, kHi
      _RS field(1-Olx:sNx+Olx,1-Oly:sNy+Oly,kSiz,nSx,nSy)
      INTEGER iRec
      INTEGER myIter
      INTEGER myThid

C Functions
c     INTEGER ILNBLNK

C     !LOCAL VARIABLES:
C Local
      LOGICAL useCurrentDir
      _RL dummyRL(1)
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

      useCurrentDir = .FALSE.
      fType='RS'
#ifdef ALLOW_MDSIO
      CALL MDS_READ_FIELD(
     I                     fName, fPrec, useCurrentDir,
     I                     fType, kSiz, kLo, kHi,
     O                     dummyRL, field,
     I                     iRec, myThid )
#endif
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: READ_REC_LEV_RL
C     !INTERFACE:
      SUBROUTINE READ_REC_LEV_RL(
     I                           fName, fPrec, kSiz, kLo, kHi,
     O                           field,
     I                           iRec, myIter, myThid )

C     !DESCRIPTION: \bv
C READ_REC_LEV_RL is a "front-end" interface to the low-level I/O routines.
C  Read a set a consecutive levels (from kLo to kHi) into 3D array (size: kSiz)
C  from record number "iRec" in binary file "fName" (precision "fPrec",
C  generally =32 or 64).
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
      INTEGER kSiz, kLo, kHi
      _RL field(1-Olx:sNx+Olx,1-Oly:sNy+Oly,kSiz,nSx,nSy)
      INTEGER iRec
      INTEGER myIter
      INTEGER myThid

C Functions
c     INTEGER ILNBLNK

C     !LOCAL VARIABLES:
C Local
      LOGICAL useCurrentDir
      _RS dummyRS(1)
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

      useCurrentDir = .FALSE.
      fType='RL'
#ifdef ALLOW_MDSIO
      CALL MDS_READ_FIELD(
     I                     fName, fPrec, useCurrentDir,
     I                     fType, kSiz, kLo, kHi,
     O                     field, dummyRS,
     I                     iRec, myThid )
#endif
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: READ_REC_XZ_RS
C     !INTERFACE:
      SUBROUTINE READ_REC_XZ_RS(
     I                           fName, fPrec, nNz,
     O                           field,
     I                           iRec, myIter, myThid )

C     !DESCRIPTION: \bv
C READ_REC_XZ_RS is a "front-end" interface to the low-level I/O
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
      INTEGER fPrec
      INTEGER nNz
      _RS field(1-Olx:sNx+Olx,nNz,nSx,nSy)
      INTEGER iRec
      INTEGER myIter
      INTEGER myThid

C Functions
c     INTEGER ILNBLNK

C     !LOCAL VARIABLES:
C Local
      LOGICAL useCurrentDir
      _RL dummyRL(1)
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

      useCurrentDir = .FALSE.
      fType='RS'
#ifdef ALLOW_MDSIO
      CALL MDS_READ_SEC_XZ(
     I                      fName, fPrec, useCurrentDir,
     I                      fType, nNz,
     O                      dummyRL, field,
     I                      iRec, myThid )
#endif
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: READ_REC_XZ_RL
C     !INTERFACE:
      SUBROUTINE READ_REC_XZ_RL(
     I                           fName, fPrec, nNz,
     O                           field,
     I                           iRec, myIter, myThid )

C     !DESCRIPTION: \bv
C READ_REC_XZ_RL is a "front-end" interface to the low-level I/O
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
      INTEGER fPrec
      INTEGER nNz
      _RL field(1-Olx:sNx+Olx,nNz,nSx,nSy)
      INTEGER iRec
      INTEGER myIter
      INTEGER myThid

C Functions
c     INTEGER ILNBLNK

C     !LOCAL VARIABLES:
C Local
      LOGICAL useCurrentDir
      _RS dummyRS(1)
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

      useCurrentDir = .FALSE.
      fType='RL'
#ifdef ALLOW_MDSIO
      CALL MDS_READ_SEC_XZ(
     I                      fName, fPrec, useCurrentDir,
     I                      fType, nNz,
     O                      field, dummyRS,
     I                      iRec, myThid )
#endif
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: READ_REC_YZ_RS
C     !INTERFACE:
      SUBROUTINE READ_REC_YZ_RS(
     I                           fName, fPrec, nNz,
     O                           field,
     I                           iRec, myIter, myThid )

C     !DESCRIPTION: \bv
C READ_REC_YZ_RS is a "front-end" interface to the low-level I/O
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
      INTEGER fPrec
      INTEGER nNz
      _RS field(1-Oly:sNy+Oly,nNz,nSx,nSy)
      INTEGER iRec
      INTEGER myIter
      INTEGER myThid

C Functions
c     INTEGER ILNBLNK

C     !LOCAL VARIABLES:
C Local
      LOGICAL useCurrentDir
      _RL dummyRL(1)
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

      useCurrentDir = .FALSE.
      fType='RS'
#ifdef ALLOW_MDSIO
      CALL MDS_READ_SEC_YZ(
     I                      fName, fPrec, useCurrentDir,
     I                      fType, nNz,
     O                      dummyRL, field,
     I                      iRec, myThid )
#endif
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: READ_REC_YZ_RL
C     !INTERFACE:
      SUBROUTINE READ_REC_YZ_RL(
     I                           fName, fPrec, nNz,
     O                           field,
     I                           iRec, myIter, myThid )

C     !DESCRIPTION: \bv
C READ_REC_YZ_RL is a "front-end" interface to the low-level I/O
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
      INTEGER fPrec
      INTEGER nNz
      _RL field(1-Oly:sNy+Oly,nNz,nSx,nSy)
      INTEGER iRec
      INTEGER myIter
      INTEGER myThid

C Functions
c     INTEGER ILNBLNK

C     !LOCAL VARIABLES:
C Local
      LOGICAL useCurrentDir
      _RS dummyRS(1)
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

      useCurrentDir = .FALSE.
      fType='RL'
#ifdef ALLOW_MDSIO
      CALL MDS_READ_SEC_YZ(
     I                      fName, fPrec, useCurrentDir,
     I                      fType, nNz,
     O                      field, dummyRS,
     I                      iRec, myThid )
#endif
      RETURN
      END
