C $Header: /u/gcmpack/MITgcm/pkg/timeave/Attic/timeave_normaliz.F,v 1.1 2001/03/06 15:51:46 jmc Exp $
C $Name:  $
#include "CPP_OPTIONS.h"

CStartofinterface
      SUBROUTINE TIMEAVE_NORMALIZ( 
     U                               fldtave, 
     I                               timeave_cumul, Ksize,
     I                               bi, bj, myThid )
C     /==========================================================\
C     | SUBROUTINE TIMEAVE_NORMALIZ                              |
C     | o Get average of field : Normalize by cumulated time     |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     == Routine arguments ==
C     myThid        - Thread number for this instance of the routine.
C     timeave_cumul - cumulated time for average
C     fldtave       - time averaged Field
C     Ksize         - 3rd dimension of local array (fldtave)
      INTEGER bi, bj, Ksize, myThid
      _RL fldtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Ksize,nSx,nSy)
      _RL timeave_cumul(Nr,nSx,nSy)

CEndofinterface

C     == Local variables ==
C     i,j,k,bi,bj  - Loop counters
      INTEGER i, j, k
 
C     DO bj = myByLo(myThid), myByHi(myThid)
C      DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,Ksize
         IF ( timeave_cumul(k,bi,bj) .NE. 0. ) THEN
          DO j=1,sNy
           DO i=1,sNx
             fldtave(i,j,k,bi,bj) = fldtave(i,j,k,bi,bj) / 
     &                              timeave_cumul(k,bi,bj)
           ENDDO
          ENDDO
         ENDIF
        ENDDO
C      ENDDO
C     ENDDO
 
      RETURN
      END
