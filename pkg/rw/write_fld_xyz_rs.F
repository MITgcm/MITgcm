C $Header: /u/gcmpack/MITgcm/pkg/rw/write_fld_xyz_rs.F,v 1.1 2001/08/17 18:34:42 adcroft Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

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
