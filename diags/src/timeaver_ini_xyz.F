C $Header: /u/gcmpack/MITgcm/diags/src/Attic/timeaver_ini_xyz.F,v 1.5 2001/02/15 19:01:39 jmc Exp $
C $Name:  $
#include "CPP_OPTIONS.h"

CStartofinterface
      SUBROUTINE TIMEAVER_INI_XYZ( fldtave, bi, bj, K, myThid )
C     /==========================================================\
C     | SUBROUTINE TIMEAVER_INI_XYZ                              |
C     | o Initialize 3D array                                    |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "AVER.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
C     fldtave - time averaged Field
      INTEGER bi, bj, K, myThid
      _RL fldtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

CEndofinterface

C     == Local variables ==
C     i,j,k,bi,bj  - Loop counters
      INTEGER i, j
 
C     DO bj = myByLo(myThid), myByHi(myThid)
C      DO bi = myBxLo(myThid), myBxHi(myThid)
C       DO k=1,Nr
         DO j=1,sNy
          DO i=1,sNx
             fldtave(i,j,k,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
C       ENDDO
C      ENDDO
C     ENDDO
 
      RETURN
      END
