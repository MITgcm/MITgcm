C $Header: /u/gcmpack/MITgcm/pkg/timeave/timeave_cumul_1k1t.F,v 1.4 2005/08/19 22:50:26 heimbach Exp $
C $Name:  $
#include "CPP_EEOPTIONS.h"

CStartofinterface
      SUBROUTINE TIMEAVE_CUMUL_1K1T( 
     O              fldtave, 
     I              fld, deltaTloc, 
     I              Ksize, k, bi, bj, myThid )
C     *==========================================================*
C     | SUBROUTINE TIMEAVE_CUMUL_1K1T
C     | o Sum over time a single level, single tile array
C     *==========================================================*
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     == Routine arguments ==
C     myThid  - Thread number for this instance of the routine.
C     fldtave - time averaged Field
C     fld     - Input Field
C     Ksize   - 3rd dimension of the time-average array (fldtave)
C     k       - current level
C     bi,bj   - current tile indices
      INTEGER Ksize
      _RL fld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fldtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Ksize,nSx,nSy)
      _RL deltaTloc
      INTEGER k, bi, bj, myThid

CEndofinterface

C     == Local variables ==
C     i,j - Loop counters
      INTEGER i, j
 
      DO j=1,sNy
        DO i=1,sNx
          fldtave(i,j,k,bi,bj) = fldtave(i,j,k,bi,bj)
     &                         + fld(i,j)*deltaTloc
        ENDDO
      ENDDO
 
      RETURN
      END
