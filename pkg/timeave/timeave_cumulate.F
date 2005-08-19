C $Header: /u/gcmpack/MITgcm/pkg/timeave/timeave_cumulate.F,v 1.4 2005/08/19 22:50:26 heimbach Exp $
C $Name:  $
#include "CPP_EEOPTIONS.h"

CStartofinterface
      SUBROUTINE TIMEAVE_CUMULATE( 
     O              fldtave, 
     I              fld, Ksize, deltaTloc, 
     I              bi, bj, myThid )
C     /==========================================================\
C     | SUBROUTINE TIMEAVE_CUMULATE                              |
C     | o Sum over time a single array                           |
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
      _RL fld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Ksize,nSx,nSy)
      _RL fldtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Ksize,nSx,nSy)
      _RL deltaTloc
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
             fldtave(i,j,k,bi,bj)=
     &       fldtave(i,j,k,bi,bj)+fld(i,j,k,bi,bj)*deltaTloc
           ENDDO
          ENDDO
         ENDDO
C       ENDDO
C      ENDDO
 
      RETURN
      END
