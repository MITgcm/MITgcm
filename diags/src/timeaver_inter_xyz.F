#include "CPP_EEOPTIONS.h"

CStartofinterface
      SUBROUTINE TIMEAVER_INTER_XYZ(
     I  thisNz,
     I  fld, fldtave,
     I  deltaT, bi, bj, K, thisK,
     I  myThid )
C     /==========================================================\
C     | SUBROUTINE TIMEAVER_INTER_XYZ                            |
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
      INTEGER thisNz, bi, bj, K, thisK, myThid
      _RL fld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,thisNz)
      _RL fldtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL deltaT

CEndofinterface

C     == Local variables ==
C     i,j,k,bi,bj  - Loop counters
      INTEGER i, j
 
C     DO bj = myByLo(myThid), myByHi(myThid)
C      DO bi = myBxLo(myThid), myBxHi(myThid)
C       DO k=1,Nr
         DO j=1,sNy
          DO i=1,sNx
           fldtave(i,j,k,bi,bj)=
     &       fldtave(i,j,k,bi,bj)+fld(i,j,thisK)*deltaT
          ENDDO
         ENDDO
C       ENDDO
C      ENDDO
C     ENDDO
 
      RETURN
      END
