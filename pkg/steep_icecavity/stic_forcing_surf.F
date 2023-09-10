#include "STIC_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: STIC_FORCING_SURF
C     !INTERFACE:
      SUBROUTINE STIC_FORCING_SURF(
     I           bi, bj, iMin, iMax, jMin, jMax,
     I           myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R STIC_FORCING_SURF
C     | o Contains problem specific surface forcing
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
c#include "DYNVARS.h"
#include "SURFACE.h"
#include "FFIELDS.h"
#include "STIC.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     bi,bj     :: Current tile indices
C     iMin,iMax :: Working range of x-index for applying forcing.
C     jMin,jMax :: Working range of y-index for applying forcing.
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number in simulation
C     myThid    :: Thread Id number
      INTEGER bi, bj
      INTEGER iMin, iMax, jMin, jMax
      _RL myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_STEEP_ICECAVITY
C     !LOCAL VARIABLES:
C     == Local variables ==
C     i,j   :: Loop counters
      INTEGER i, j
      LOGICAL SHI_useRealFWflux
CEOP

      SHI_useRealFWflux = useRealFreshWaterFlux .AND.
     &  ( .NOT.STICboundaryLayer .OR. SHI_withBL_realFWflux )

c     DO bj=myByLo(myThid),myByHi(myThid)
c      DO bi=myBxLo(myThid),myBxHi(myThid)

C--   Zero out surface forcing terms below ice-shelf
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          IF ( kTopC(i,j,bi,bj).NE.0 ) THEN
            surfaceForcingT(i,j,bi,bj) = 0.
            surfaceForcingS(i,j,bi,bj) = 0.
            EmPmR(i,j,bi,bj) = 0.
            Qsw  (i,j,bi,bj) = 0.
C-    just for consistent diagnostics, also reset Qnet:
            Qnet (i,j,bi,bj) = 0.
          ENDIF
         ENDDO
        ENDDO
        DO j=1-OLy,sNy+OLy
         DO i=2-OLx,sNx+OLx
          IF ( MAX( kTopC(i-1,j,bi,bj), kTopC(i,j,bi,bj) ).NE.0 ) THEN
            surfaceForcingU(i,j,bi,bj) = 0.
          ENDIF
         ENDDO
        ENDDO
        DO j=2-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          IF ( MAX( kTopC(i,j-1,bi,bj), kTopC(i,j,bi,bj) ).NE.0 ) THEN
            surfaceForcingV(i,j,bi,bj) = 0.
          ENDIF
         ENDDO
        ENDDO

#ifdef EXACT_CONSERV
        IF ( staggerTimeStep ) THEN
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             PmEpR(i,j,bi,bj) = -EmPmR(i,j,bi,bj)
           ENDDO
          ENDDO
        ENDIF
#endif /* EXACT_CONSERV */

        IF ( usingZCoords ) THEN
          DO j = jMin, jMax
           DO i = iMin, iMax
             phi0surf(i,j,bi,bj) = phi0surf(i,j,bi,bj)
     &         + shelficeLoadAnomaly(i,j,bi,bj)*recip_rhoConst
           ENDDO
          ENDDO
        ENDIF

#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             shelficeDragU(i,j,bi,bj) = 0.
             shelficeDragV(i,j,bi,bj) = 0.
           ENDDO
          ENDDO
         ENDIF
#endif /* ALLOW_DIAGNOSTICS */

c      ENDDO
c     ENDDO

#endif /* ALLOW_STEEP_ICECAVITY */
      RETURN
      END
