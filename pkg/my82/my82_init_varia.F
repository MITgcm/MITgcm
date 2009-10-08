C $Header: /u/gcmpack/MITgcm/pkg/my82/my82_init_varia.F,v 1.3 2009/10/08 20:07:53 jmc Exp $
C $Name:  $

#include "MY82_OPTIONS.h"

      SUBROUTINE MY82_INIT_VARIA( myThid )
C     *==========================================================*
C     | SUBROUTINE MY82_INIT_VARIA                               |
C     | o Routine to initialize MY82 parameters and variables.   |
C     *==========================================================*
C     | Initialize MY92 parameters and variables.                |
C     *==========================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "MY82.h"

C     === Routine arguments ===
C     myThid -  Number of this instance of MY_INIT
      INTEGER myThid

#ifdef ALLOW_MY82

C     === Local variables ===
C     i,j,k,bi,bj - Loop counters

      INTEGER i, j, k, bi, bj
      _RL gam1, gam2

C     magic parameters( M. Satoh, p. 314)
      gam1 = 1. _d 0/3. _d 0 - 2. _d 0 *A1/B1
      gam2 = (B2+6. _d 0*A1)/B1
      alpha1 = 3. _d 0 *A2*gam1
      alpha2 = 3. _d 0 *A2*(gam1+gam2)
      beta1  = A1*B1*(gam1-C1)
      beta2  = A1*(B1*(gam1-C1) + 6. _d 0 *A1 + 3. _d 0 *A2)
      beta3  = A2*B1*gam1
      beta4  = A2*(B1*(gam1+gam2) - 3. _d 0 *A1)

C-----------------------------------------------------------------------
C Initialize MY82 variables MYviscAr, MYdiffKrT
C-----------------------------------------------------------------------

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k = 1, Nr
         DO j = 1-OLy, sNy+OLy
          DO i = 1-OLx, sNx+OLx
           MYhbl    (i,j  ,bi,bj) = 0. _d 0
           MYviscAr (i,j,k,bi,bj) = viscArNr(k)
           MYdiffKr (i,j,k,bi,bj) = diffKrNrT(k)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#endif /* ALLOW_MY82 */

      RETURN
      END
