C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_get_velocity.F,v 1.3 2012/03/06 15:37:24 jmc Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"
#ifdef ALLOW_SEAICE
# include "SEAICE_OPTIONS.h"
#endif /* ALLOW_SEAICE */


CBOP
C !ROUTINE: THSICE_GET_VELOCITY

C !INTERFACE: ==========================================================
      SUBROUTINE THSICE_GET_VELOCITY(
     O                  uLoc, vLoc,
     I                  bi, bj, myTime, myIter, myThid )

C !DESCRIPTION: \bv
C     *===========================================================*
C     | SUBROUTINE THSICE_GET_VELOCITY
C     | o load seaice velocity from pkg/seaice common block
C     *===========================================================*
C \ev

C !USES: ===============================================================
      IMPLICIT NONE

C     === Global variables ===

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
c#include "THSICE_SIZE.h"
c#include "THSICE_PARAMS.h"
#ifdef ALLOW_SEAICE
# include "SEAICE_SIZE.h"
# include "SEAICE.h"
#endif /* ALLOW_SEAICE */

C !INPUT PARAMETERS: ===================================================
C     === Routine arguments ===
C     uLoc/vLoc :: current ice velocity on C-grid [m/s]
C     bi,bj     :: Tile indices
C     myTime    :: Current time in simulation (s)
C     myIter    :: Current iteration number
C     myThid    :: My Thread Id number
      _RL     uLoc(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     vLoc(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER bi,bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_THSICE
C !LOCAL VARIABLES: ====================================================
C     === Local variables ===
C     i,j,      :: Loop counters
      INTEGER i, j
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|


#ifdef ALLOW_SEAICE
      IF ( useSEAICE ) THEN
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          uLoc(i,j) = UICE(i,j,bi,bj)
          vLoc(i,j) = VICE(i,j,bi,bj)
         ENDDO
        ENDDO
      ELSE
#else /* ALLOW_SEAICE */
      IF ( .TRUE. ) THEN
#endif /* ALLOW_SEAICE */
C-    set ice velocity to zero
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          uLoc(i,j) = 0.
          vLoc(i,j) = 0.
         ENDDO
        ENDDO
      ENDIF

#endif /* ALLOW_THSICE */

      RETURN
      END
