#include "CPP_EEOPTIONS.h"

CStartofinterface
      SUBROUTINE TIMEAVER_FINAL_XY( fldtave, timeave, bi, bj, myThid )
C     /==========================================================\
C     | SUBROUTINE TIMEAVER_FINAL_XY                             |
C     | o Get final average of field                             |
C     \==========================================================/

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
C     timeave - time averaged
C     fldtave - time averaged Field
      INTEGER bi, bj, myThid
      _RL fldtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      REAL timeave(Nz,nSx,nSy)

CEndofinterface

C     == Local variables ==
C     i,j,k,bi,bj  - Loop counters
      INTEGER i, j

      write(0,*) 'aja: final=',fldtave(1,12,bi,bj)
 
C     DO bj = myByLo(myThid), myByHi(myThid)
C      DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
             fldtave(i,j,bi,bj) = fldtave(i,j,bi,bj) / timeave(1,bi,bj)
          ENDDO
         ENDDO
C      ENDDO
C     ENDDO
 
      RETURN
      END
