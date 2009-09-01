C $Header: /u/gcmpack/MITgcm/pkg/rw/write_glvec_rl.F,v 1.2 2009/09/01 19:28:24 jmc Exp $
C $Name:  $

#include "RW_OPTIONS.h"

CBOP
C !ROUTINE: WRITE_GLVEC_RL
C !INTERFACE:
      SUBROUTINE WRITE_GLVEC_RL(
     I           pref, suff, array, sizArr, myIter, myThid )

C !DESCRIPTION:
C WRITE_GLVEC_RL
C  is a "front-end" interface to the low-level MDS I/O routines. 
C  Writes a global (=tile independent) vector.

C !USES:
      IMPLICIT NONE

C == Global Variables
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C !INPUT PARAMETERS:
      CHARACTER*(*) pref,suff
      INTEGER sizArr
      _RL     array(sizArr)
      INTEGER myIter
      INTEGER myThid

C !FUNCTIONS:
      INTEGER  ILNBLNK, IFNBLNK
      EXTERNAL ILNBLNK, IFNBLNK

C !LOCAL VARIABLES:
      _RS     dummyRS(1)
      CHARACTER*(2) fType
      INTEGER ioUnit, bi, bj, iRec
      INTEGER s1Lo,s1Hi,s2Lo,s2Hi
      CHARACTER*(MAX_LEN_FNAM) fullName
CEOP

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

      ioUnit = 0
      fType ='RL'
      bi = 0
      bj = 0
      iRec = 1
#ifdef ALLOW_MDSIO
      CALL MDS_WRITEVEC_LOC(
     I                       fullName, writeBinaryPrec, ioUnit,
     I                       fType, sizArr, array, dummyRS,
     I                       bi, bj, iRec, myIter, myThid )
#endif

      RETURN
      END
