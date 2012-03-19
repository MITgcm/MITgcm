C $Header: /u/gcmpack/MITgcm/pkg/icefront/icefront_init_varia.F,v 1.8 2012/03/19 21:45:29 dimitri Exp $
C $Name:  $

#include "ICEFRONT_OPTIONS.h"

CBOP
      SUBROUTINE ICEFRONT_INIT_VARIA( myThid )
C     /============================================================\
C     | SUBROUTINE ICEFRONT_INIT_VARIA                             |
C     | o Routine to initialize ICEFRONT variables.                |
C     |============================================================|
C     | Initialize ICEFRONT parameters and variables.              |
C     \============================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "ICEFRONT.h"
#ifdef ALLOW_SUBGLACIAL_RUNOFF
# include "EXF_PARAM.h"
#endif

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
         DO J = 1-OLy, sNy+OLy
          DO I = 1-OLx, sNx+OLx
           icefront_TendT (i,j,k,bi,bj) = 0. _d 0
           icefront_TendS (i,j,k,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#ifdef ALLOW_SUBGLACIAL_RUNOFF
      call exf_init_runoff (
     &     sgrunofffile, sgrunoffperiod,
     &     exf_inscal_sgrunoff, sgrunoffmask, sgrunoffconst,
     &     sgrunoff, sgrunoff0, sgrunoff1, mythid )
#endif /* ALLOW_SUBGLACIAL_RUNOFF */

#endif /* ALLOW_ICEFRONT */

      RETURN
      END
