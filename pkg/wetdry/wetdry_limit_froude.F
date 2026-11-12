#include "WETDRY_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: WETDRY_LIMIT_FROUDE

C !INTERFACE:
      SUBROUTINE WETDRY_LIMIT_FROUDE( myTime, myIter, myThid )

C !DESCRIPTION:
C     Froude-number cap (wdMaxFroude > 0), applied to the new velocity
C     u(n+1) at the start of WETDRY_LIMIT_OUTFLOW, so before continuity:
C     on every open interior face, each velocity component is limited to
C        |u| <= wdMaxFroude * sqrt( g D )
C     with D the flow depth of the face as for the Manning drag (the
C     higher of the two surfaces minus the face bed crest, at least
C     wdMinDepth). Water pouring off a bank into a deep channel runs as
C     a thin supercritical sheet on a 100 m grid (tens of m/s within a
C     few steps in a breakup flood); the cap keeps it near critical.
C     Continuity and tracer transports use the capped velocity, so volume
C     and tracers stay exactly conserved; momentum is not. Open-boundary
C     faces (maskInW/S = 0) are left alone. Done over the whole tile,
C     overlaps included, so neighbouring tiles cap a face alike.

C !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"
#include "DYNVARS.h"
#include "WETDRY.h"

C !INPUT PARAMETERS:
C     myTime :: time at the beginning of the current time step
C     myIter :: iteration number at the beginning of the time step
C     myThid :: my Thread Id number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_WETDRY
C !LOCAL VARIABLES:
      INTEGER i, j, k, bi, bj
      _RL zbf, hf, cap

      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=2-OLx,sNx+OLx
          IF ( maskInW(i,j,bi,bj).NE.zeroRS ) THEN
           zbf = MAX( R_low(i-1,j,bi,bj), R_low(i,j,bi,bj) )
           hf  = MAX( Ro_surf(i-1,j,bi,bj) + etaN(i-1,j,bi,bj),
     &                Ro_surf(i,j,bi,bj)   + etaN(i,j,bi,bj) ) - zbf
           cap = wdMaxFroude*SQRT( gravity*MAX( hf, wdMinDepth ) )
           DO k=1,Nr
            uVel(i,j,k,bi,bj) = MAX( -cap,
     &                         MIN( cap, uVel(i,j,k,bi,bj) ) )
           ENDDO
          ENDIF
         ENDDO
        ENDDO
        DO j=2-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          IF ( maskInS(i,j,bi,bj).NE.zeroRS ) THEN
           zbf = MAX( R_low(i,j-1,bi,bj), R_low(i,j,bi,bj) )
           hf  = MAX( Ro_surf(i,j-1,bi,bj) + etaN(i,j-1,bi,bj),
     &                Ro_surf(i,j,bi,bj)   + etaN(i,j,bi,bj) ) - zbf
           cap = wdMaxFroude*SQRT( gravity*MAX( hf, wdMinDepth ) )
           DO k=1,Nr
            vVel(i,j,k,bi,bj) = MAX( -cap,
     &                         MIN( cap, vVel(i,j,k,bi,bj) ) )
           ENDDO
          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#endif /* ALLOW_WETDRY */

      RETURN
      END
