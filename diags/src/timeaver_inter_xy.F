#include "CPP_EEOPTIONS.h"

CStartofinterface
      SUBROUTINE TIMEAVER_INTER_XY(
     I  fld, fldtave,
     I  deltaT, bi, bj,
     I  myThid )
C     /==========================================================\
C     | SUBROUTINE TIMEAVER_INTER_XY                             |
C     | o Sum over time a single array                           |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "AVER.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
C     fld  - Input Field
C     fldtave - time averaged Field
      INTEGER bi, bj, myThid
      _RL fld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fldtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL deltaT

CEndofinterface

C     == Local variables ==
C     i,j,k,bi,bj  - Loop counters
      INTEGER i, j
 
C     DO bj = myByLo(myThid), myByHi(myThid)
C      DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           fldtave(i,j,bi,bj)=
     &       fldtave(i,j,bi,bj)+fld(i,j)*deltaT
          ENDDO
         ENDDO
C      ENDDO
C     ENDDO
 
      RETURN
      END
