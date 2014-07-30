C $Header: /u/gcmpack/MITgcm/pkg/kl10/kl10_init_varia.F,v 1.1 2014/07/30 03:28:05 jmc Exp $
C $Name:  $

#include "KL10_OPTIONS.h"

      SUBROUTINE KL10_INIT_VARIA( myThid )
C     *==========================================================*
C     | SUBROUTINE KL10_INIT_VARIA
C     | o Routine to initialize KL10 parameters and variables.
C     *==========================================================*
C     | Initialize KL10 parameters and variables.
C     *==========================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
c#include "GRID.h"
#include "KL10.h"

C     === Routine arguments ===
C     myThid -  Number of this instance of KL10_INIT
      INTEGER myThid

#ifdef ALLOW_KL10

C     === Local variables ===
C     i,j,k,bi,bj - Loop counters

      INTEGER i, j, k, bi, bj

C-----------------------------------------------------------------------
C Initialize KL variables KLviscAr, KLdiffKrT
C-----------------------------------------------------------------------

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k = 1, Nr
         DO j = 1-OLy, sNy+OLy
          DO i = 1-OLx, sNx+OLx
           KLviscAr (i,j,k,bi,bj) = viscArNr(k)
           KLdiffKr (i,j,k,bi,bj) = diffKrNrT(k)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#endif /* ALLOW_KL10 */

      RETURN
      END
