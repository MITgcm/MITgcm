#include "STIC_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: STIC_INIT_VARIA

C !INTERFACE:
      SUBROUTINE STIC_INIT_VARIA( myThid )

C     !DESCRIPTION:
C     Initialize STEEP_ICECAVITY variables

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "STIC.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid ::  my Thread Id number
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER i,j,k,bi,bj

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C     Initialize arrays in common blocks :
C     3-dim.
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           icfHeatFlux      (i,j,k,bi,bj) = 0. _d 0
           icfFreshWaterFlux(i,j,k,bi,bj) = 0. _d 0
           icfForcingT      (i,j,k,bi,bj) = 0. _d 0
           icfForcingS      (i,j,k,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO

C     2-dim.
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          sticfHeatFlux      (i,j,bi,bj) = 0. _d 0
          sticfFreshWaterFlux(i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO

C- end bi,bj loops
       ENDDO
      ENDDO

      RETURN
      END
