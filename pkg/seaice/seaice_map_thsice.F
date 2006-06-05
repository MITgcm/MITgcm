C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_map_thsice.F,v 1.1 2006/06/05 22:46:50 mlosch Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"
#ifdef ALLOW_THSICE
# include "THSICE_OPTIONS.h"
#endif /* ALLOW_THSICE */

CStartOfInterface
      SUBROUTINE SEAICE_MAP_THSICE( myTime, myIter, myThid )
C     /==========================================================\
C     | SUBROUTINE SEAICE_MAP_THSICE                             |
C     | o Map thermodynamic thsice variable iceMask and          |
C     |   to seaice variables HEFF and AREA                      |
C     |==========================================================|
C     | written by Martin Losch, June 2006                       |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "SEAICE.h"
#ifdef ALLOW_THSICE
# include "THSICE_VARS.h"
#endif /* ALLOW_THSICE */

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

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          AREA(i,j,1,bi,bj) = iceMask(i,j,bi,bj)
          HEFF(i,j,1,bi,bj) = iceMask(i,j,bi,bj)*iceHeight(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO   
      ENDDO

#endif /* ALLOW_THSICE */
      RETURN
      END
