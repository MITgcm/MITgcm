C $Header: /u/gcmpack/MITgcm/pkg/timeave/timeave_norma_2v.F,v 1.2 2003/12/05 02:27:58 jmc Exp $
C $Name:  $
#include "CPP_EEOPTIONS.h"

CStartofinterface
      SUBROUTINE TIMEAVE_NORMA_2V( 
     U                               fldtave,
     I                               cumulWeight, Ksize,
     I                               bi, bj, myThid )
C     /==========================================================\
C     | SUBROUTINE TIMEAVE_NORMA_2V                              |
C     | o Get average of field : Normalize by cumulated weight   |
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
      _RL cumulWeight(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Ksize,nSx,nSy)

CEndofinterface

C     == Local variables ==
C     i,j,k,bi,bj  - Loop counters
      INTEGER i, j, k
 
C     DO bj = myByLo(myThid), myByHi(myThid)
C      DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,Ksize
         DO j=1,sNy
          DO i=1,sNx
           IF ( cumulWeight(i,j,k,bi,bj) .GT. 0. _d 0 ) THEN
             fldtave(i,j,k,bi,bj) = fldtave(i,j,k,bi,bj)
     &                            / cumulWeight(i,j,k,bi,bj)
           ENDIF
          ENDDO
         ENDDO
        ENDDO
C      ENDDO
C     ENDDO
 
      RETURN
      END
