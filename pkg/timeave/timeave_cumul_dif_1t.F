C $Header: /u/gcmpack/MITgcm/pkg/timeave/timeave_cumul_dif_1t.F,v 1.4 2014/08/14 16:47:25 jmc Exp $
C $Name:  $
#include "TIMEAVE_OPTIONS.h"

CStartofinterface
      SUBROUTINE TIMEAVE_CUMUL_DIF_1T(
     O   fldtave,
     I   fld1, fld2, Ksize, dir, deltaTloc,
     I   bi, bj, myThid )
C     /==========================================================*
C     | SUBROUTINE TIMEAVE_CUMUL_DIF_1T
C     | o Sum over time the product:
C     |   minus gradient of 1rst field  x  2nd field (1 tile)
C     \==========================================================*
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"

C     == Routine arguments ==
C     myThid  - Thread number for this instance of the routine.
C     fldtave - time averaged Field
C     fld1,fld2  - Input Field
C     dir     - define the gradient direction: 1: X dir ; 2: Y dir ; 3: R dir
C     Ksize   - 3rd dimension of local arrays (Input and Output fields)
      INTEGER Ksize, dir
      _RL fld1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Ksize)
      _RL fld2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Ksize)
      _RL fldtave(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Ksize,nSx,nSy)
      _RL deltaTloc
      INTEGER bi, bj, myThid

CEndofinterface

#ifdef ALLOW_TIMEAVE
C     == Local variables ==
C     i,j,k,bi,bj  - Loop counters
      INTEGER i, j, k

      IF ( dir.eq.1 )  THEN

        DO k=1,Ksize
         DO j=1,sNy
          DO i=1,sNx
           fldtave(i,j,k,bi,bj) = fldtave(i,j,k,bi,bj)
     &        - ( fld1(i,j,k) - fld1(i-1,j,k) )
     &          * recip_dxC(i,j,bi,bj)
     &          * fld2(i,j,k) * deltaTloc
          ENDDO
         ENDDO
        ENDDO

      ELSEIF ( dir.eq.2 ) THEN

        DO k=1,Ksize
         DO j=1,sNy
          DO i=1,sNx
           fldtave(i,j,k,bi,bj) = fldtave(i,j,k,bi,bj)
     &        - ( fld1(i,j,k) - fld1(i,j-1,k) )
     &          * recip_dyC(i,j,bi,bj)
     &          * fld2(i,j,k) * deltaTloc
          ENDDO
         ENDDO
        ENDDO

      ELSEIF ( dir.eq.3 ) THEN

        DO k=2,Ksize
         DO j=1,sNy
          DO i=1,sNx
           fldtave(i,j,k,bi,bj) = fldtave(i,j,k,bi,bj)
     &        + ( fld1(i,j,k) - fld1(i,j,k-1) )
     &          * recip_drC(k)
     &          * fld2(i,j,k) * deltaTloc
          ENDDO
         ENDDO
        ENDDO

      ENDIF

#endif /* ALLOW_TIMEAVE */

      RETURN
      END
