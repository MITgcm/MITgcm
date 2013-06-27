C $Header: /u/gcmpack/MITgcm/model/src/calc_oce_mxlayer.F,v 1.10 2013/06/27 14:47:01 m_bates Exp $
C $Name:  $

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
C     i,j :: Loop counters
      INTEGER i,j,k
      LOGICAL calcMixLayerDepth
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
     &       .OR. GM_useK3D
      ENDIF
#endif
#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics.AND. .NOT.calcMixLayerDepth ) THEN
        calcMixLayerDepth = DIAGNOSTICS_IS_ON('MXLDEPTH',myThid)
      ENDIF
#endif
      IF ( calcMixLayerDepth ) THEN

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
     I            bi, bj, 1-OLx, sNx+OLx, 1-OLy, sNy+OLy, 1, 1,
     O            rhoMxL, myThid )

        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
           rhoKm1(i,j) = rhoSurf(i,j)
           rhoMxL(i,j) = rhoSurf(i,j)
     &                 + MAX( rhoMxL(i,j)*hMixCriteria, dRhoSmall )
           hMixLayer(i,j,bi,bj) = rF(1)-R_low(i,j,bi,bj)
         ENDDO
        ENDDO
        DO k = 2,Nr
C-    potential density (reference level = surface level)
         CALL FIND_RHO_2D(
     I        1-OLx, sNx+OLx, 1-OLy, sNy+OLy, 1,
     I        theta(1-OLx,1-OLy,k,bi,bj), salt(1-OLx,1-OLy,k,bi,bj),
     O        rhoLoc,
     I        k, bi, bj, myThid )

         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           IF ( k.LE.klowC(i,j,bi,bj) .AND.
     &          rhoLoc(i,j).GE.rhoMxL(i,j) ) THEN
             IF ( rhoLoc(i,j).GT.rhoKm1(i,j) ) THEN
              tmpFac = ( rhoMxL(i,j) - rhoKm1(i,j) )
     &               / ( rhoLoc(i,j) - rhoKm1(i,j) )
             ELSE
              tmpFac = 0.
             ENDIF
             hMixLayer(i,j,bi,bj) = rF(1)-rC(k-1)+tmpFac*drC(k)
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
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
           IF ( klowC(i,j,bi,bj) .GT. 0 ) THEN
            hMixLayer(i,j,bi,bj) = drF(1)
            rhoMxL(i,j) = 1.
           ELSE
            hMixLayer(i,j,bi,bj) = rF(1)
            rhoMxL(i,j) = -1.
           ENDIF
         ENDDO
        ENDDO
        DO k = 2,Nr-1
C-    potential density (reference level = surface level)
         CALL FIND_RHO_2D(
     I        1-OLx, sNx+OLx, 1-OLy, sNy+OLy, 1,
     I        theta(1-OLx,1-OLy,k,bi,bj), salt(1-OLx,1-OLy,k,bi,bj),
     O        rhoLoc,
     I        k, bi, bj, myThid )

         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           IF ( k.LT.klowC(i,j,bi,bj)
     &          .AND. rhoMxL(i,j).GE.0. ) THEN
             sigmAv = ( rhoLoc(i,j)-rhoSurf(i,j)+dRhoSmall )
     &              / ( rC(1)-rC(k) )
             IF ( -sigmaR(i,j,k+1).GT.sigmAv*hMixCriteria ) THEN
               tmpFac = 0. _d 0
               IF ( sigmAv.GT.0. _d 0 ) THEN
                 tmpFac = hMixCriteria*sigmaR(i,j,k)/sigmaR(i,j,k+1)
                 IF ( tmpFac .GT. 1. _d 0 ) THEN
                   tmpFac = 1. _d 0
     &             + ( tmpFac - 1. _d 0 )/( hMixCriteria - 1. _d 0 )
                 ENDIF
                 tmpFac = MAX( 0. _d 0, MIN( tmpFac, 2. _d 0 ) )
               ENDIF
               hMixLayer(i,j,bi,bj) = rF(1)-rF(k+1)
     &                              - drF(k)*tmpFac*0.5 _d 0
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
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1
            rhoLoc(i,j)=(hMixSmooth *   hMixLayer(i,j,bi,bj)   +
     &                       tmpFac * ( hMixLayer(i-1,j,bi,bj) +
     &                                  hMixLayer(i+1,j,bi,bj) +
     &                                  hMixLayer(i,j-1,bi,bj) +
     &                                  hMixLayer(i,j+1,bi,bj) )
     &                  )
     &                 /(hMixSmooth +
     &                       tmpFac * ( maskC(i-1,j,1,bi,bj) + 
     &                                  maskC(i+1,j,1,bi,bj) +
     &                                  maskC(i,j-1,1,bi,bj) + 
     &                                  maskC(i,j+1,1,bi,bj) )
     &                  ) * maskC(i,j,1,bi,bj)
         ENDDO
        ENDDO
        DO j=1-Oly+1,sNy+Oly-1
         DO i=1-Olx+1,sNx+Olx-1
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
