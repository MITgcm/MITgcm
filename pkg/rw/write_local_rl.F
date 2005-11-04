C $Header: /u/gcmpack/MITgcm/pkg/rw/write_local_rl.F,v 1.4 2005/11/04 01:33:50 jmc Exp $
C $Name:  $

#include "RW_OPTIONS.h"

      SUBROUTINE WRITE_LOCAL_RL(
     I          pref,suff,nNr,field,bi,bj,iRec,myIter,myThid)
C WRITE_LOCAL_RL is a "front-end" interface to the low-level I/O
C routines. It assumes single record files.
      IMPLICIT NONE
C Global
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
C Arguments
      CHARACTER*(*) pref,suff
      INTEGER nNr
      _RL field(1-Olx:sNx+Olx,1-Oly:sNy+Oly,nNr)
      INTEGER bi,bj
      INTEGER iRec
      INTEGER myIter
      INTEGER myThid
C Functions
      INTEGER ILNBLNK,IFNBLNK
C Common
      COMMON /RD_WR_FLD/ globalFile
      LOGICAL globalFile
C Local
      CHARACTER*(2) fType
      INTEGER s1Lo,s1Hi,s2Lo,s2Hi
      CHARACTER*(80) fullName

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
C
      fType='RL'
#ifdef ALLOW_MDSIO
      IF (nSx.EQ.1.AND.nSy.EQ.1) THEN
C The hack below replaces MDS_WRITELOCAL with MDSWRITEFIELD_LOC for
C single-threaded execution because the former does not support the
C singleCpuIo option. This is a placeholder until MDS_WRITELOCAL
C functionality is superseded by pkg/diagnostics.
         CALL MDSWRITEFIELD_LOC(
     &        fullName, writeBinaryPrec, globalFile,
     &        fType, nNr, field, iRec, myIter, myThid )
      ELSE
         CALL MDS_WRITELOCAL(
     &        fullName, writeBinaryPrec, globalFile,
     &        fType, nNr, field, bi, bj, iRec, myIter, myThid )
      ENDIF
#endif
      RETURN
      END
