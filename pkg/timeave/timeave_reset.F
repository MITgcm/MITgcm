C $Header: /u/gcmpack/MITgcm/pkg/timeave/timeave_reset.F,v 1.2 2003/10/09 04:19:20 edhill Exp $
C $Name:  $
#include "TIMEAVE_OPTIONS.h"

CStartofinterface
      SUBROUTINE TIMEAVE_RESET( fldtave, Ksize, bi, bj, myThid )
C     /==========================================================\
C     | SUBROUTINE TIMEAVE_RESET                                 |
C     | o Initialize 3D array  (3rd_Dim is an argument)          |
C     |   Can be applied to 2D array with 3rd_Dim = 1            |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
C     fldtave - time averaged Field
C     Ksize - 3rd dimension of local arrays "fldtave"
      INTEGER bi, bj, Ksize, myThid
      _RL fldtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Ksize,nSx,nSy)

CEndofinterface

C     == Local variables ==
C     i,j,k,bi,bj  - Loop counters
      INTEGER i, j, k
 
c     DO bj = myByLo(myThid), myByHi(myThid)
c      DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,Ksize
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
             fldtave(i,j,k,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
c      ENDDO
c     ENDDO
 
      RETURN
      END
