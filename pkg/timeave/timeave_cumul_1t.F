C $Header: /u/gcmpack/MITgcm/pkg/timeave/timeave_cumul_1t.F,v 1.5 2005/08/19 22:50:26 heimbach Exp $
C $Name:  $
#include "CPP_EEOPTIONS.h"

CStartofinterface
      SUBROUTINE TIMEAVE_CUMUL_1T( 
     O              fldtave, 
     I              fld, Ksize, deltaTloc, 
     I              bi, bj, myThid )
C     /==========================================================\
C     | SUBROUTINE TIMEAVE_CUMUL_1T                              |
C     | o Sum over time a single local(tile) array               |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
C     fldtave - time averaged Field
C     fld  - Input Field
C     Ksize - 3rd dimension of local arrays (Input and Output fields)
      INTEGER Ksize
      _RL fld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Ksize)
      _RL fldtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Ksize,nSx,nSy)
      _RL deltaTloc
      INTEGER bi, bj, myThid

CEndofinterface

C     == Local variables ==
C     i,j,k,bi,bj  - Loop counters
      INTEGER i, j, k
 
        DO k=1,Ksize
         DO j=1,sNy
          DO i=1,sNx
             fldtave(i,j,k,bi,bj)=
     &       fldtave(i,j,k,bi,bj)+fld(i,j,k)*deltaTloc
           ENDDO
          ENDDO
         ENDDO
 
      RETURN
      END
