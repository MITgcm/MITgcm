C $Header: /u/gcmpack/MITgcm/pkg/timeave/Attic/timeave_cumul_fc.F,v 1.1 2002/01/03 15:56:52 jmc Exp $
C $Name:  $
#include "CPP_OPTIONS.h"

CStartofinterface
      SUBROUTINE TIMEAVE_CUMUL_FC( 
     O              fldtave, 
     I              fld, Ksize, deltaT, 
     I              bi, bj, myThid )
C     /==========================================================\
C     | SUBROUTINE TIMEAVE_CUMUL_FC                              |
C     | o Sum over time a single RS array into a RL array        |
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
      _RS fld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Ksize,nSx,nSy)
      _RL fldtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Ksize,nSx,nSy)
      _RL deltaT
      INTEGER bi, bj, myThid

CEndofinterface

C     == Local variables ==
C     i,j,k,bi,bj  - Loop counters
      INTEGER i, j, k
 
C     DO bj = myByLo(myThid), myByHi(myThid)
C      DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,Ksize
         DO j=1,sNy
          DO i=1,sNx
           fldtave(i,j,k,bi,bj) = fldtave(i,j,k,bi,bj)
     &                       + deltaT*fld(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
C       ENDDO
C      ENDDO
 
      RETURN
      END
