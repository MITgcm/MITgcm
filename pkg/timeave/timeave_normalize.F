C $Header: /u/gcmpack/MITgcm/pkg/timeave/timeave_normalize.F,v 1.1 2009/12/28 01:03:17 jmc Exp $
C $Name:  $

#include "CPP_EEOPTIONS.h"

CBOP 0
C !ROUTINE: TIMEAVE_NORMALIZE

C !INTERFACE:
      SUBROUTINE TIMEAVE_NORMALIZE(
     U                   fldtave,
     I                   timeave_cumul, Ksize,
     I                   bi, bj, myThid )

C !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE TIMEAVE_NORMALIZE
C     | o Get average of field : Normalize by cumulated time
C     *==========================================================*

C !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"

C !INPUT/OUTPUT PARAMETERS:
C     fldtave       :: time averaged Field
C     timeave_cumul :: cumulated time for average
C     Ksize         :: 3rd dimension of local array (fldtave)
C     bi, bj        :: indices of tile to process
C     myThid        :: my Thread Id number
      INTEGER Ksize
      _RL fldtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Ksize,nSx,nSy)
      _RL timeave_cumul(nSx,nSy)
      INTEGER bi, bj
      INTEGER myThid
CEOP

C !LOCAL VARIABLES:
C     i,j,k    :: Loop counters
      INTEGER i, j, k

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C     DO bj = myByLo(myThid), myByHi(myThid)
C      DO bi = myBxLo(myThid), myBxHi(myThid)
        IF ( timeave_cumul(bi,bj) .NE. 0. _d 0 ) THEN
         DO k=1,Ksize
          DO j=1,sNy
           DO i=1,sNx
             fldtave(i,j,k,bi,bj) = fldtave(i,j,k,bi,bj)
     &                            / timeave_cumul(bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDIF
C      ENDDO
C     ENDDO

      RETURN
      END
