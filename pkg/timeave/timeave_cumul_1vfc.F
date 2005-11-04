C $Header: /u/gcmpack/MITgcm/pkg/timeave/timeave_cumul_1vfc.F,v 1.5 2005/11/04 01:35:33 jmc Exp $
C $Name:  $
#include "CPP_EEOPTIONS.h"

CStartofinterface
      SUBROUTINE TIMEAVE_CUMUL_1VFC( 
     O   fldtave,
     I   fld1, fld2, Ksize, deltaTloc,
     I   bi, bj, myThid )
C     /==========================================================\
C     | SUBROUTINE TIMEAVE_CUMUL_1VFC                            |
C     | o Sum over time a product of 1 field  (RL array)         |
C     |                          and 1 factor (RS array)         |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
C     fldtave - time averaged Field
C     fld1  - Input Field
C     fld2  - Input Factor
C     Ksize - 3rd dimension of local arrays (Input and Output fields)
      INTEGER Ksize
      _RL fld1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Ksize,nSx,nSy)
      _RS fld2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Ksize,nSx,nSy)
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
           fldtave(i,j,k,bi,bj)= fldtave(i,j,k,bi,bj)
     &       + deltaTloc*fld1(i,j,k,bi,bj)*fld2(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
C       ENDDO
C      ENDDO

      RETURN
      END
