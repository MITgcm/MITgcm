C $Header: /u/gcmpack/MITgcm/diags/src/Attic/timeaver_final_xy.F,v 1.9 2001/02/15 19:01:39 jmc Exp $
C $Name:  $
#include "CPP_OPTIONS.h"

CStartofinterface
      SUBROUTINE TIMEAVER_FINAL_XY( fldtave, timeave, bi, bj, myThid )
C     /==========================================================\
C     | SUBROUTINE TIMEAVER_FINAL_XY                             |
C     | o Get final average of field                             |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
C     timeave - time averaged
C     fldtave - time averaged Field
      INTEGER bi, bj, myThid
      _RL fldtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL timeave(Nr,nSx,nSy)

CEndofinterface

C     == Local variables ==
C     i,j,k,bi,bj  - Loop counters
      INTEGER i, j

C     DO bj = myByLo(myThid), myByHi(myThid)
C      DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
           IF ( timeave(1,bi,bj) .NE. 0. ) THEN
             fldtave(i,j,bi,bj) = fldtave(i,j,bi,bj) / timeave(1,bi,bj)
           ENDIF
          ENDDO
         ENDDO
C      ENDDO
C     ENDDO
 
      RETURN
      END
