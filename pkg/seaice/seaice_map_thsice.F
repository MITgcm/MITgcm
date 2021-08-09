#include "SEAICE_OPTIONS.h"
#ifdef ALLOW_THSICE
# include "THSICE_OPTIONS.h"
#endif /* ALLOW_THSICE */
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CStartOfInterface
      SUBROUTINE SEAICE_MAP_THSICE( myTime, myIter, myThid )
C     *==========================================================*
C     | SUBROUTINE SEAICE_MAP_THSICE                             |
C     | o Map thermodynamic thsice variable iceMask and          |
C     |   to seaice variables HEFF and AREA                      |
C     *==========================================================*
C     | written by Martin Losch, June 2006                       |
C     *==========================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "SEAICE_SIZE.h"
#include "SEAICE.h"
#ifdef ALLOW_THSICE
# include "THSICE_VARS.h"
#endif /* ALLOW_THSICE */
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     === Routine arguments ===
C     myTime - Simulation time
C     myIter - Simulation timestep number
C     myThid - Thread no. that called this routine.
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEndOfInterface

#ifdef ALLOW_THSICE
C     === Local variables ===
C     i,j,bi,bj - Loop counters

      INTEGER i, j, bi, bj

# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE icemask   = comlev1, key=ikey_dynamics, kind=isbyte
CADJ STORE iceheight = comlev1, key=ikey_dynamics, kind=isbyte
#endif

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          AREA(i,j,bi,bj) = iceMask(i,j,bi,bj)
          HEFF(i,j,bi,bj) = iceMask(i,j,bi,bj)*iceHeight(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#endif /* ALLOW_THSICE */
      RETURN
      END
