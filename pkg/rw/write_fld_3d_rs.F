C $Header: /u/gcmpack/MITgcm/pkg/rw/write_fld_3d_rs.F,v 1.3 2009/09/01 19:28:24 jmc Exp $
C $Name:  $

#include "RW_OPTIONS.h"

      SUBROUTINE WRITE_FLD_3D_RS(
     I                 pref, suff, nNz, field, myIter, myThid )
C WRITE_FLD_3D_RS is a "front-end" interface to the low-level I/O
C routines. It assumes single record files.
      IMPLICIT NONE
C Global
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
C Arguments
      CHARACTER*(*) pref,suff
      INTEGER nNz
      _RS field(1-Olx:sNx+Olx,1-Oly:sNy+Oly,nNz,nSx,nSy)
      INTEGER myIter
      INTEGER myThid
C Functions
      INTEGER ILNBLNK,IFNBLNK
C Common
      COMMON /RD_WR_FLD/ globalFile
      LOGICAL globalFile
C Local
      LOGICAL useCurrentDir
      _RL dummyRL(1)
      CHARACTER*(2) fType
      INTEGER iRec
      INTEGER s1Lo,s1Hi,s2Lo,s2Hi
      CHARACTER*(MAX_LEN_FNAM) fullName
C
C--   Build file name
C     Name has form 'prefix.suffix'
C     e.g. U.0000000100
      s1Lo = IFNBLNK(pref)
      s1Hi = ILNBLNK(pref)
      IF ( suff .EQ. ' ' ) THEN
       WRITE( fullName, '(A)' ) pref(s1Lo:s1Hi)
      ELSEIF ( suff .EQ. 'I10' ) THEN
       WRITE( fullName, '(A,A,I10.10)' ) pref(s1Lo:s1Hi),'.',myIter
      ELSE
       s2Lo = IFNBLNK(suff)
       s2Hi = ILNBLNK(suff)
       WRITE( fullName, '(A,A)' ) pref(s1Lo:s1Hi),suff(s2Lo:s2Hi)
      ENDIF

      useCurrentDir = .FALSE.
      fType='RS'
      iRec=1
#ifdef ALLOW_MDSIO
      CALL MDS_WRITE_FIELD(
     I                      fullName, writeBinaryPrec,
     I                      globalFile, useCurrentDir,
     I                      fType, nNz, 1, nNz, dummyRL, field,
     I                      iRec, myIter, myThid )
#endif
      RETURN
      END
