#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CALC_OCE_MXLAYER
C     !INTERFACE:
      SUBROUTINE CALC_OCE_MXLAYER(
     I                       rhoSurf, sigmaR,
     I                       bi, bj, myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R CALC_OCE_MXLAYER
C     | o Diagnose the Oceanic surface Mixed-Layer
C     | Note: output "hMixLayer" is in "r" unit, i.e., in Pa
C     |       when using P-coordinate.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#ifdef ALLOW_GMREDI
# include "GMREDI.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     rhoSurf   :: Surface density anomaly
C     sigmaR    :: Vertical gradient of potential density
C     bi,bj     :: tile indices
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number in simulation
C     myThid    :: my Thread Id number
      _RL     rhoSurf(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     sigmaR (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     === Functions ====
#ifdef ALLOW_DIAGNOSTICS
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON
#endif /* ALLOW_DIAGNOSTICS */

C     !LOCAL VARIABLES:
C     == Local variables ==
C     i, j, k   :: Loop counters
C     kSrf      :: surface center-level index
C     kTop      :: surface interface level index
C     kDir      :: downward k increment
C     kU, kL    :: loop ranges, Up and Low
C     kup, klw  :: interface level index above & below current level k
      INTEGER i, j, k
      INTEGER kSrf, kTop, kDir, deltaK
      INTEGER kU, kL, klw, kup
      LOGICAL calcMixLayerDepth, kIn
      INTEGER method
      _RL     rhoBigNb
      _RL     rhoMxL(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     rhoKm1(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     rhoLoc(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     tmpFac, sigmAv
CEOP

      calcMixLayerDepth = .FALSE.
#ifdef ALLOW_GMREDI
      IF ( useGMRedi .AND. .NOT.useKPP ) THEN
       calcMixLayerDepth = GM_useSubMeso .OR. GM_taper_scheme.EQ.'fm07'
     &       .OR. GM_useBatesK3d
      ENDIF
#endif
#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics.AND. .NOT.calcMixLayerDepth ) THEN
        calcMixLayerDepth = DIAGNOSTICS_IS_ON('MXLDEPTH',myThid)
      ENDIF
#endif
      IF ( calcMixLayerDepth ) THEN

        IF ( usingPCoords ) THEN
         kTop    = Nr+1
         kSrf    = Nr
         kDir    = -1
         deltaK  =  1
        ELSE
         kTop    =  1
         kSrf    =  1
         kDir    =  1
         deltaK  =  0
        ENDIF
C--   Select which "method" to use:
       method = 0
       IF ( hMixCriteria.LT.0. ) method = 1
       IF ( hMixCriteria.GT.1. ) method = 2

       IF ( method.EQ.1 ) THEN

C--   First method :
C     where the potential density (ref.lev=surface) is larger than
C       surface density plus Delta_Rho = hMixCriteria * Alpha(surf)
C     = density of water which is -hMixCriteria colder than surface water
C     (see Kara, Rochford, and Hurlburt JGR 2000 for default criterion)

c       hMixCriteria  = -0.8 _d 0
c       dRhoSmall = 1. _d -6
        rhoBigNb  = rhoConst*1. _d 10
        CALL FIND_ALPHA(
     I            bi, bj, 1-OLx, sNx+OLx, 1-OLy, sNy+OLy, kSrf, kSrf,
     O            rhoMxL, myThid )

        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           rhoKm1(i,j) = rhoSurf(i,j)
           rhoMxL(i,j) = rhoSurf(i,j)
     &                 + MAX( rhoMxL(i,j)*hMixCriteria, dRhoSmall )
           hMixLayer(i,j,bi,bj) = Ro_surf(i,j,bi,bj) - R_low(i,j,bi,bj)
         ENDDO
        ENDDO
C-    Z-coord: (kU,kL,kDir) = (2,Nr,1) ; P-coord: (kU,kL,kDir)= (Nr-1,1,-1)
        kU = 2 + deltaK*(Nr-3)
        kL = Nr - deltaK*(Nr-1)
        DO k = kU,kL,kDir
C-    potential density (reference level = surface level)
         CALL FIND_RHO_2D(
     I        1-OLx, sNx+OLx, 1-OLy, sNy+OLy, kSrf,
     I        theta(1-OLx,1-OLy,k,bi,bj), salt(1-OLx,1-OLy,k,bi,bj),
     O        rhoLoc,
     I        k, bi, bj, myThid )

         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
c          kIn = k.LT.klowC(i,j,bi,bj).AND.k.GE.kSurfC(i,j,bi,bj)
           kIn = k.LE.klowC(i,j,bi,bj).AND.k.GE.kSurfC(i,j,bi,bj)
           IF ( kIn .AND. rhoLoc(i,j).GE.rhoMxL(i,j) ) THEN
             IF ( rhoLoc(i,j).GT.rhoKm1(i,j) ) THEN
              tmpFac = ( rhoMxL(i,j) - rhoKm1(i,j) )
     &               / ( rhoLoc(i,j) - rhoKm1(i,j) )
             ELSE
              tmpFac = 0.
             ENDIF
C-    Note: sign is more related to kDir than it is to gravity orientation
c            hMixLayer(i,j,bi,bj) = ( rF(kTop)-rC(k-kDir) )*kDir
             hMixLayer(i,j,bi,bj) = -gravitySign*( rF(kTop)-rC(k-kDir) )
     &                            + tmpFac*drC(k+deltaK)
             rhoMxL(i,j) = rhoBigNb
           ELSE
             rhoKm1(i,j) = rhoLoc(i,j)
           ENDIF
          ENDDO
         ENDDO
        ENDDO

       ELSEIF ( method.EQ.2 ) THEN

C--   Second method :
C     where the local stratification exceed the mean stratification above
C     (from surface down to here) by factor hMixCriteria

c       hMixCriteria  = 1.5 _d 0
c       dRhoSmall = 1. _d -2
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          IF ( klowC(i,j,bi,bj) .GT. 0 ) THEN
           hMixLayer(i,j,bi,bj) = drF(kSrf)
           rhoMxL(i,j) = 1.
          ELSE
           hMixLayer(i,j,bi,bj) = rF(kTop)
           rhoMxL(i,j) = -1.
          ENDIF
         ENDDO
        ENDDO
C-    Z-coord: (kU,kL,kDir) = (2,Nr-1,1) ; P-coord: (kU,kL,kDir)= (Nr-1,2,-1)
        kU = 2 + deltaK*(Nr-3)
        kL = Nr-1 - deltaK*(Nr-3)
        DO k = kU,kL,kDir
C-    potential density (reference level = surface level)
         CALL FIND_RHO_2D(
     I        1-OLx, sNx+OLx, 1-OLy, sNy+OLy, kSrf,
     I        theta(1-OLx,1-OLy,k,bi,bj), salt(1-OLx,1-OLy,k,bi,bj),
     O        rhoLoc,
     I        k, bi, bj, myThid )

         kup = k+deltaK
         klw = kup+kDir
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           kIn = k.LT.klowC(i,j,bi,bj).AND.k.GT.kSurfC(i,j,bi,bj)
           IF ( kIn .AND. rhoMxL(i,j).GE.0. ) THEN
             sigmAv = ( rhoLoc(i,j)-rhoSurf(i,j)+dRhoSmall )
     &              / ( rC(k)-rC(kSrf) ) * gravitySign
             IF ( gravitySign * sigmaR(i,j,klw)
     &            .GT.sigmAv*hMixCriteria ) THEN
               tmpFac = 0. _d 0
               IF ( sigmAv.GT.0. _d 0 ) THEN
                 tmpFac = hMixCriteria*sigmaR(i,j,kup)/sigmaR(i,j,klw)
                 IF ( tmpFac .GT. 1. _d 0 ) THEN
                   tmpFac = 1. _d 0
     &             + ( tmpFac - 1. _d 0 )/( hMixCriteria - 1. _d 0 )
                 ENDIF
                 tmpFac = MAX( 0. _d 0, MIN( tmpFac, 2. _d 0 ) )
               ENDIF
C-    Note: sign is more related to kDir than it is to gravity orientation
               hMixLayer(i,j,bi,bj) = -gravitySign *
     &                               ( rF(kTop)-rF(klw) )
     &                               - drF(k)*tmpFac*0.5 _d 0
               rhoMxL(i,j) = -1.
             ENDIF
           ENDIF
          ENDDO
         ENDDO
        ENDDO

       ELSE
        STOP 'S/R CALC_OCE_MXLAYER: invalid method'
       ENDIF

       IF ( hMixSmooth .GT. 0. _d 0 ) THEN
        tmpFac = (1. _d 0 - hMixSmooth ) / 4. _d 0
        DO j=1-OLy+1,sNy+OLy-1
         DO i=1-OLx+1,sNx+OLx-1
            rhoLoc(i,j)=(hMixSmooth *   hMixLayer(i,j,bi,bj)   +
     &                       tmpFac * ( hMixLayer(i-1,j,bi,bj) +
     &                                  hMixLayer(i+1,j,bi,bj) +
     &                                  hMixLayer(i,j-1,bi,bj) +
     &                                  hMixLayer(i,j+1,bi,bj) )
     &                  )
     &                 /(hMixSmooth +
     &                       tmpFac * ( maskC(i-1,j,kSrf,bi,bj) +
     &                                  maskC(i+1,j,kSrf,bi,bj) +
     &                                  maskC(i,j-1,kSrf,bi,bj) +
     &                                  maskC(i,j+1,kSrf,bi,bj) )
     &                  ) * maskC(i,j,kSrf,bi,bj)
         ENDDO
        ENDDO
        DO j=1-OLy+1,sNy+OLy-1
         DO i=1-OLx+1,sNx+OLx-1
            hMixLayer(i,j,bi,bj) = rhoLoc(i,j)
         ENDDO
        ENDDO
       ENDIF

#ifdef ALLOW_DIAGNOSTICS
       IF ( useDiagnostics ) THEN
        CALL DIAGNOSTICS_FILL( hMixLayer, 'MXLDEPTH',
     &                         0, 1, 1, bi, bj, myThid )
       ENDIF
#endif /* ALLOW_DIAGNOSTICS */

C--   end if calcMixLayerDepth
      ENDIF

      RETURN
      END
