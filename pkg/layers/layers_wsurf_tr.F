#include "CPP_OPTIONS.h"
#include "LAYERS_OPTIONS.h"

CBOP
C     !ROUTINE: LAYERS_WSURF_TR
C     !INTERFACE:
      SUBROUTINE LAYERS_WSURF_TR(thetaFld, saltFld, wVelFld,
     I                         myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE LAYERS_WSURF_TR
C     |     (copied from CALC_WSURF_TR)
C     | o Compute a correction for the source/sink of tracer
C     |   due to the linear free surface.
C     | o Add this correction to the layers surface flux
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"
#include "LAYERS_SIZE.h"
#include "LAYERS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime   :: Current time in simulation
C     myIter   :: Current iteration number in simulation
C     myThid   :: Thread number for this instance of the routine.
C     thetaFld :: Potential Temperature field
C     saltFld  :: Salinity field
C     wvelFld  :: vertical velocity field
      _RL myTime
      INTEGER myIter
      INTEGER myThid
      _RL thetaFld(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL saltFld (1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL wVelFld (1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)

#ifdef LAYERS_THERMODYNAMICS

C     !LOCAL VARIABLES:
C     Local variables
C     i,j,k,bi,bj  :: loop counter
C     tCor, sCor   :: corrections for theta and salt
      INTEGER i,j,bi,bj,ks
      _RL tcor, scor
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
         DO j=1,sNy
          DO i=1,sNx
             ks = ksurfC(i,j,bi,bj)
             IF (ks.LE.Nr) THEN
c --           Temperature
               tcor = -wVelFld(i,j,ks,bi,bj) * thetaFld(i,j,ks,bi,bj)
               scor = -wVelFld(i,j,ks,bi,bj) * saltFld(i,j,ks,bi,bj)
               IF (linFSConserveTr) THEN
                 tcor = tcor + TsurfCor
                 scor = scor + SsurfCor
               ENDIF
c --           Temperature needs to be in the units of TFLUX)
               layers_surfflux(i,j,1,1,bi,bj) =
     &           layers_surfflux(i,j,1,1,bi,bj) +
     &           tcor * HeatCapacity_Cp * rUnit2mass
c --           Salinity needs to be in the units of SFLUX)
               layers_surfflux(i,j,1,2,bi,bj) =
     &           layers_surfflux(i,j,1,2,bi,bj) +
     &           scor * rUnit2mass
             ENDIF
          ENDDO
         ENDDO
C-     end bi,bj loop.
       ENDDO
      ENDDO

#endif /* LAYERS_THERMODYNAMICS */

      RETURN
      END
