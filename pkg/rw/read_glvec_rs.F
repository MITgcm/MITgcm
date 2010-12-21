C $Header: /u/gcmpack/MITgcm/pkg/rw/read_glvec_rs.F,v 1.1 2010/12/21 23:44:45 jmc Exp $
C $Name:  $

#include "RW_OPTIONS.h"

CBOP
C !ROUTINE: READ_GLVEC_RS
C !INTERFACE:
      SUBROUTINE READ_GLVEC_RS(
     I                          pref, suff,
     O                          array,
     I                          sizArr, iRec, myThid )

C !DESCRIPTION:
C READ_GLVEC_RS
C  is a "front-end" interface to the low-level MDS I/O routines.
C  Reads a global (=tile independent) vector.

C !USES:
      IMPLICIT NONE

C == Global Variables
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C !INPUT PARAMETERS:
      CHARACTER*(*) pref,suff
      INTEGER sizArr
      _RS     array(sizArr)
      INTEGER iRec
      INTEGER myThid

C !FUNCTIONS:
      INTEGER  ILNBLNK, IFNBLNK
      EXTERNAL ILNBLNK, IFNBLNK

C !LOCAL VARIABLES:
      _RL     dummyRL(1)
      CHARACTER*(2) fType
      INTEGER ioUnit, bi, bj
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
      fType ='RS'
      bi = 0
      bj = 0
#ifdef ALLOW_MDSIO
      CALL MDS_READVEC_LOC(
     &                       fullName, readBinaryPrec, ioUnit,
     &                       fType, sizArr, dummyRL, array,
     I                       bi, bj, iRec, myThid )
#else
      STOP 'ABNORMAL END: S/R READ_GLVEC_RS needs MDSIO pkg'
#endif

      RETURN
      END
