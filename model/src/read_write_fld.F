C $Header: /u/gcmpack/MITgcm/model/src/Attic/read_write_fld.F,v 1.2 2001/02/04 14:38:48 cnh Exp $
C $Name:  $
#include "CPP_OPTIONS.h"

C--  File read_write_fld.F: Routines to handle mid-level I/O interface.
C--   Contents
C--   o SET_WRITE_GLOBAL_FLD
C--   o READ_FLD_XY_RS
C--   o READ_FLD_XY_RL
C--   o READ_FLD_XYZ_RS
C--   o READ_FLD_XYZ_RL
C--   o WRITE_FLD_XY_RS
C--   o WRITE_FLD_XY_RL
C--   o WRITE_FLD_XYZ_RS
C--   o WRITE_FLD_XYZ_RL

      SUBROUTINE SET_WRITE_GLOBAL_FLD ( flag )
      IMPLICIT NONE
C SET_WRITE_GLOBAL_FLD( flag ) sets an internal logical state to
C indicate whether files written by subsequent call to the
C READ_WRITE_FLD package should create "global" or "tiled" files.
C   flag = .TRUE.  indicates "global" files
C   flag = .FALSE. indicates "tiled" files
C
C Arguments
      LOGICAL flag
C Common
      COMMON /RD_WR_FLD/ globalFile
      LOGICAL globalFile
C
      globalFile=flag
C
      RETURN
      END

      SUBROUTINE READ_FLD_XY_RS( pref,suff,field,myIter,myThid)
C READ_FLD_XY_RS is a "front-end" interface to the low-level I/O
C routines. It assumes single record files.
      IMPLICIT NONE
C Global
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
C Arguments
      CHARACTER*(*) pref,suff
      _RS field(1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      INTEGER myIter
      INTEGER myThid
C Functions
      INTEGER ILNBLNK,IFNBLNK
C Local
      CHARACTER*(2) fType
      INTEGER nNz
      INTEGER irec
      INTEGER s1Lo,s1Hi,s2Lo,s2Hi
      CHARACTER*(80) fullName
C
C--   Build file name
C     Name has form 'prefix.suffix'
C     e.g. U.0000000100
      s1Lo = IFNBLNK(pref)
      s1Hi = ILNBLNK(pref)
      IF ( suff .EQ. ' ' ) THEN
       WRITE( fullName, '(A)' ) pref(s1Lo:s1Hi)
      ELSE
       s2Lo = IFNBLNK(suff)
       s2Hi = ILNBLNK(suff)
       WRITE( fullName, '(A,A)' ) pref(s1Lo:s1Hi),suff(s2Lo:s2Hi)
      ENDIF
C
      fType='RS'
      nNz=1
      irec=1
      CALL MDSREADFIELD( fullName, readBinaryPrec, fType,
     &     nNz, field, irec, myThid )
C
      RETURN
      END

      SUBROUTINE READ_FLD_XY_RL( pref,suff,field,myIter,myThid)
C READ_FLD_XY_RL is a "front-end" interface to the low-level I/O
C routines. It assumes single record files.
      IMPLICIT NONE
C Global
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
C Arguments
      CHARACTER*(*) pref,suff
      _RL field(1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      INTEGER myIter
      INTEGER myThid
C Functions
      INTEGER ILNBLNK,IFNBLNK
C Local
      CHARACTER*(2) fType
      INTEGER nNz
      INTEGER irec
      INTEGER s1Lo,s1Hi,s2Lo,s2Hi
      CHARACTER*(80) fullName
C
C--   Build file name
C     Name has form 'prefix.suffix'
C     e.g. U.0000000100
      s1Lo = IFNBLNK(pref)
      s1Hi = ILNBLNK(pref)
      IF ( suff .EQ. ' ' ) THEN
       WRITE( fullName, '(A)' ) pref(s1Lo:s1Hi)
      ELSE
       s2Lo = IFNBLNK(suff)
       s2Hi = ILNBLNK(suff)
       WRITE( fullName, '(A,A)' ) pref(s1Lo:s1Hi),suff(s2Lo:s2Hi)
      ENDIF
C
      fType='RL'
      nNz=1
      irec=1
      CALL MDSREADFIELD( fullName, readBinaryPrec, fType,
     &     nNz, field, irec, myThid )
C
      RETURN
      END

      SUBROUTINE READ_FLD_XYZ_RS( pref,suff,field,myIter,myThid)
C READ_FLD_XYZ_RS is a "front-end" interface to the low-level I/O
C routines. It assumes single record files.
      IMPLICIT NONE
C Global
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
C Arguments
      CHARACTER*(*) pref,suff
      _RS field(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      INTEGER myIter
      INTEGER myThid
C Functions
      INTEGER ILNBLNK,IFNBLNK
C Local
      CHARACTER*(2) fType
      INTEGER nNz
      INTEGER irec
      INTEGER s1Lo,s1Hi,s2Lo,s2Hi
      CHARACTER*(80) fullName
C
C--   Build file name
C     Name has form 'prefix.suffix'
C     e.g. U.0000000100
      s1Lo = IFNBLNK(pref)
      s1Hi = ILNBLNK(pref)
      IF ( suff .EQ. ' ' ) THEN
       WRITE( fullName, '(A)' ) pref(s1Lo:s1Hi)
      ELSE
       s2Lo = IFNBLNK(suff)
       s2Hi = ILNBLNK(suff)
       WRITE( fullName, '(A,A)' ) pref(s1Lo:s1Hi),suff(s2Lo:s2Hi)
      ENDIF
C
      fType='RS'
      nNz=Nr
      irec=1
      CALL MDSREADFIELD( fullName, readBinaryPrec, fType,
     &     nNz, field, irec, myThid )
C
      RETURN
      END

      SUBROUTINE READ_FLD_XYZ_RL( pref,suff,field,myIter,myThid)
C READ_FLD_XYZ_RL is a "front-end" interface to the low-level I/O
C routines. It assumes single record files.
      IMPLICIT NONE
C Global
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
C Arguments
      CHARACTER*(*) pref,suff
      _RL field(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      INTEGER myIter
      INTEGER myThid
C Functions
      INTEGER ILNBLNK,IFNBLNK
C Local
      CHARACTER*(2) fType
      INTEGER nNz
      INTEGER irec
      INTEGER s1Lo,s1Hi,s2Lo,s2Hi
      CHARACTER*(80) fullName
C
C--   Build file name
C     Name has form 'prefix.suffix'
C     e.g. U.0000000100
      s1Lo = IFNBLNK(pref)
      s1Hi = ILNBLNK(pref)
      IF ( suff .EQ. ' ' ) THEN
       WRITE( fullName, '(A)' ) pref(s1Lo:s1Hi)
      ELSE
       s2Lo = IFNBLNK(suff)
       s2Hi = ILNBLNK(suff)
       WRITE( fullName, '(A,A)' ) pref(s1Lo:s1Hi),suff(s2Lo:s2Hi)
      ENDIF
C
      fType='RL'
      nNz=Nr
      irec=1
      CALL MDSREADFIELD( fullName, readBinaryPrec, fType,
     &     nNz, field, irec, myThid )
C
      RETURN
      END

      SUBROUTINE WRITE_FLD_XY_RS( pref,suff,field,myIter,myThid)
C WRITE_FLD_XY_RS is a "front-end" interface to the low-level I/O
C routines. It assumes single record files.
      IMPLICIT NONE
C Global
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
C Arguments
      CHARACTER*(*) pref,suff
      _RS field(1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      INTEGER myIter
      INTEGER myThid
C Functions
      INTEGER ILNBLNK,IFNBLNK
C Common
      COMMON /RD_WR_FLD/ globalFile
      LOGICAL globalFile
C Local
      CHARACTER*(2) fType
      INTEGER nNz
      INTEGER irec
      INTEGER s1Lo,s1Hi,s2Lo,s2Hi
      CHARACTER*(80) fullName
C
C--   Build file name
C     Name has form 'prefix.suffix'
C     e.g. U.0000000100
      s1Lo = IFNBLNK(pref)
      s1Hi = ILNBLNK(pref)
      IF ( suff .EQ. ' ' ) THEN
       WRITE( fullName, '(A)' ) pref(s1Lo:s1Hi)
      ELSE
       s2Lo = IFNBLNK(suff)
       s2Hi = ILNBLNK(suff)
       WRITE( fullName, '(A,A)' ) pref(s1Lo:s1Hi),suff(s2Lo:s2Hi)
      ENDIF
C
      fType='RS'
      nNz=1
      irec=1
      CALL MDSWRITEFIELD( fullName, writeBinaryPrec, globalFile,
     &     fType, nNz, field, irec, myIter, myThid )
C
      RETURN
      END

      SUBROUTINE WRITE_FLD_XY_RL( pref,suff,field,myIter,myThid)
C WRITE_FLD_XY_RL is a "front-end" interface to the low-level I/O
C routines. It assumes single record files.
      IMPLICIT NONE
C Global
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
C Arguments
      CHARACTER*(*) pref,suff
      _RL field(1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      INTEGER myIter
      INTEGER myThid
C Functions
      INTEGER ILNBLNK,IFNBLNK
C Common
      COMMON /RD_WR_FLD/ globalFile
      LOGICAL globalFile
C Local
      CHARACTER*(2) fType
      INTEGER nNz
      INTEGER irec
      INTEGER s1Lo,s1Hi,s2Lo,s2Hi
      CHARACTER*(80) fullName
C
C--   Build file name
C     Name has form 'prefix.suffix'
C     e.g. U.0000000100
      s1Lo = IFNBLNK(pref)
      s1Hi = ILNBLNK(pref)
      IF ( suff .EQ. ' ' ) THEN
       WRITE( fullName, '(A)' ) pref(s1Lo:s1Hi)
      ELSE
       s2Lo = IFNBLNK(suff)
       s2Hi = ILNBLNK(suff)
       WRITE( fullName, '(A,A)' ) pref(s1Lo:s1Hi),suff(s2Lo:s2Hi)
      ENDIF
C
      fType='RL'
      nNz=1
      irec=1
      CALL MDSWRITEFIELD( fullName, writeBinaryPrec, globalFile,
     &     fType, nNz, field, irec, myIter, myThid )
C
      RETURN
      END

      SUBROUTINE WRITE_FLD_XYZ_RS( pref,suff,field,myIter,myThid)
C WRITE_FLD_XYZ_RS is a "front-end" interface to the low-level I/O
C routines. It assumes single record files.
      IMPLICIT NONE
C Global
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
C Arguments
      CHARACTER*(*) pref,suff
      _RS field(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      INTEGER myIter
      INTEGER myThid
C Functions
      INTEGER ILNBLNK,IFNBLNK
C Common
      COMMON /RD_WR_FLD/ globalFile
      LOGICAL globalFile
C Local
      CHARACTER*(2) fType
      INTEGER nNz
      INTEGER irec
      INTEGER s1Lo,s1Hi,s2Lo,s2Hi
      CHARACTER*(80) fullName
C
C--   Build file name
C     Name has form 'prefix.suffix'
C     e.g. U.0000000100
      s1Lo = IFNBLNK(pref)
      s1Hi = ILNBLNK(pref)
      IF ( suff .EQ. ' ' ) THEN
       WRITE( fullName, '(A)' ) pref(s1Lo:s1Hi)
      ELSE
       s2Lo = IFNBLNK(suff)
       s2Hi = ILNBLNK(suff)
       WRITE( fullName, '(A,A)' ) pref(s1Lo:s1Hi),suff(s2Lo:s2Hi)
      ENDIF
C
      fType='RS'
      nNz=Nr
      irec=1
      CALL MDSWRITEFIELD( fullName, writeBinaryPrec, globalFile,
     &     fType, nNz, field, irec, myIter, myThid )
C
      RETURN
      END

      SUBROUTINE WRITE_FLD_XYZ_RL( pref,suff,field,myIter,myThid)
C WRITE_FLD_XYZ_RL is a "front-end" interface to the low-level I/O
C routines. It assumes single record files.
      IMPLICIT NONE
C Global
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
C Arguments
      CHARACTER*(*) pref,suff
      _RL field(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      INTEGER myIter
      INTEGER myThid
C Functions
      INTEGER ILNBLNK,IFNBLNK
C Common
      COMMON /RD_WR_FLD/ globalFile
      LOGICAL globalFile
C Local
      CHARACTER*(2) fType
      INTEGER nNz
      INTEGER irec
      INTEGER s1Lo,s1Hi,s2Lo,s2Hi
      CHARACTER*(80) fullName
C
C--   Build file name
C     Name has form 'prefix.suffix'
C     e.g. U.0000000100
      s1Lo = IFNBLNK(pref)
      s1Hi = ILNBLNK(pref)
      IF ( suff .EQ. ' ' ) THEN
       WRITE( fullName, '(A)' ) pref(s1Lo:s1Hi)
      ELSE
       s2Lo = IFNBLNK(suff)
       s2Hi = ILNBLNK(suff)
       WRITE( fullName, '(A,A)' ) pref(s1Lo:s1Hi),suff(s2Lo:s2Hi)
      ENDIF
C
      fType='RL'
      nNz=Nr
      irec=1
      CALL MDSWRITEFIELD( fullName, writeBinaryPrec, globalFile,
     &     fType, nNz, field, irec, myIter, myThid )
C
      RETURN
      END
