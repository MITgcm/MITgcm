C $Header: /u/gcmpack/MITgcm/pkg/pp81/pp81_init_varia.F,v 1.2 2009/10/08 20:08:20 jmc Exp $
C $Name:  $

#include "PP81_OPTIONS.h"

      SUBROUTINE PP81_INIT_VARIA( myThid )
C     *==========================================================*
C     | SUBROUTINE PP81_INIT_VARIA
C     | o Routine to initialize PP81 parameters and variables.
C     *==========================================================*
C     | Initialize PP81 parameters and variables.
C     *==========================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
c#include "GRID.h"
#include "PP81.h"

C     === Routine arguments ===
C     myThid -  Number of this instance of PP81_INIT
      INTEGER myThid

#ifdef ALLOW_PP81

C     === Local variables ===
C     i,j,k,bi,bj - Loop counters

      INTEGER i, j, k, bi, bj

C-----------------------------------------------------------------------
C Initialize PP variables PPviscAr, PPdiffKrT
C-----------------------------------------------------------------------

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k = 1, Nr
         DO j = 1-OLy, sNy+OLy
          DO i = 1-OLx, sNx+OLx
           PPviscAr (i,j,k,bi,bj) = viscArNr(k)
           PPdiffKr (i,j,k,bi,bj) = diffKrNrT(k)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#endif /* ALLOW_PP81 */

      RETURN
      END
