C $Header: /u/gcmpack/MITgcm/diags/src/Attic/timeaver_ini_xy.F,v 1.4 2001/02/15 19:01:39 jmc Exp $
C $Name:  $
#include "CPP_OPTIONS.h"

CStartofinterface
      SUBROUTINE TIMEAVER_INI_XY( fldtave, bi, bj, myThid )
C     /==========================================================\
C     | SUBROUTINE TIMEAVER_INI_XY                               |
C     | o Initialize 2D array                                    |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "AVER.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
C     fldtave - time averaged Field
      INTEGER bi, bj, myThid
      _RL fldtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

CEndofinterface

C     == Local variables ==
C     i,j,k,bi,bj  - Loop counters
      INTEGER i, j
 
C     DO bj = myByLo(myThid), myByHi(myThid)
C      DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
             fldtave(i,j,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
C      ENDDO
C     ENDDO
 
      RETURN
      END
