C $Header: /u/gcmpack/MITgcm/pkg/icefront/icefront_init_varia.F,v 1.10 2013/11/10 02:58:34 yunx Exp $
C $Name:  $

#include "ICEFRONT_OPTIONS.h"

CBOP
      SUBROUTINE ICEFRONT_INIT_VARIA( myThid )
C     *============================================================*
C     | SUBROUTINE ICEFRONT_INIT_VARIA                             |
C     | o Routine to initialize ICEFRONT variables.                |
C     *============================================================*
C     | Initialize ICEFRONT parameters and variables.              |
C     *============================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "ICEFRONT.h"

C     === Routine arguments ===
C     myThid -  Number of this instance of ICEFRONT_INIT_VARIA
      INTEGER myThid
CEndOfInterface

#ifdef ALLOW_ICEFRONT
C     === Local variables ===
C     I,J,K,bi,bj - Loop counters
      INTEGER I, J, K, bi, bj
CEOP

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO K = 1, Nr
         DO J = 1, sNy
          DO I = 1, sNx
           icefront_TendT (i,j,k,bi,bj) = 0. _d 0
           icefront_TendS (i,j,k,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#endif /* ALLOW_ICEFRONT */

      RETURN
      END
