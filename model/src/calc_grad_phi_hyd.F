C $Header: /u/gcmpack/MITgcm/model/src/calc_grad_phi_hyd.F,v 1.13 2010/09/10 17:23:17 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CALC_GRAD_PHI_HYD
C     !INTERFACE:
      SUBROUTINE CALC_GRAD_PHI_HYD(
     I                       k, bi, bj, iMin,iMax, jMin,jMax,
     I                       phiHydC, alphRho, tFld, sFld,
     O                       dPhiHydX, dPhiHydY,
     I                       myTime, myIter, myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R CALC_GRAD_PHI_HYD
C     | o Calculate the gradient of Hydrostatic potential anomaly
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"
#include "DYNVARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     bi,bj      :: tile index
C     iMin,iMax,jMin,jMax :: Loop counters
C     phiHydC    :: Hydrostatic Potential anomaly
C                  (atmos: =Geopotential ; ocean-z: =Pressure/rho)
C     alphRho    :: Density (z-coord) or specific volume (p-coord)
C     tFld       :: Potential temp.
C     sFld       :: Salinity
C     dPhiHydX,Y :: Gradient (X & Y directions) of Hyd. Potential
C     myTime :: Current time
C     myIter :: Current iteration number
C     myThid :: Instance number for this call of the routine.
      INTEGER k, bi,bj, iMin,iMax, jMin,jMax
      _RL phiHydC(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL alphRho(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL sFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL dPhiHydX(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL dPhiHydY(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL myTime
      INTEGER myIter, myThid

#ifdef INCLUDE_PHIHYD_CALCULATION_CODE

C     !LOCAL VARIABLES:
C     == Local variables ==
C     i,j :: Loop counters
      INTEGER i,j
      _RL varLoc(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
#ifdef NONLIN_FRSURF
      _RL factorZ, factorP, conv_theta2T
      _RL factPI
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#endif
CEOP

#ifdef NONLIN_FRSURF
      IF (select_rStar.GE.2 .AND. nonlinFreeSurf.GE.4 ) THEN
# ifndef DISABLE_RSTAR_CODE
C-    Integral of b.dr = rStarFac * Integral of b.dr* :
C      and will add later (select_rStar=2) the contribution of
C      the slope of the r* coordinate.
       IF ( buoyancyRelation .EQ. 'ATMOSPHERIC' ) THEN
C-     Consistent with Phi'= Integr[ theta'.dPi ] :
        DO j=jMin,jMax
         DO i=iMin,iMax
          varLoc(i,j) = phiHydC(i,j)*rStarFacC(i,j,bi,bj)**atm_kappa
     &                + phi0surf(i,j,bi,bj)
         ENDDO
        ENDDO
       ELSE
        DO j=jMin,jMax
         DO i=iMin,iMax
          varLoc(i,j) = phiHydC(i,j)*rStarFacC(i,j,bi,bj)
     &                + phi0surf(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDIF
      ELSEIF (select_rStar.GE.1 .AND. nonlinFreeSurf.GE.4 ) THEN
C-    Integral of b.dr but scaled to correspond to a fixed r-level (=r*)
C      no contribution of the slope of the r* coordinate (select_rStar=1)
       IF ( buoyancyRelation .EQ. 'ATMOSPHERIC' ) THEN
C-     Consistent with Phi'= Integr[ theta'.dPi ] :
        DO j=jMin,jMax
         DO i=iMin,iMax
          IF (Ro_surf(i,j,bi,bj).EQ.rC(k)) THEN
           factPI=atm_Cp*( ((etaH(i,j,bi,bj)+rC(k))/atm_Po)**atm_kappa
     &                    -(                 rC(k) /atm_Po)**atm_kappa
     &                  )
           varLoc(i,j) = factPI*alphRho(i,j)
     &                 + phi0surf(i,j,bi,bj)
          ELSEIF (Ro_surf(i,j,bi,bj).NE.0. _d 0) THEN
           factPI = (rC(k)/Ro_surf(i,j,bi,bj))**atm_kappa
           varLoc(i,j) = phiHydC(i,j)
     &                  *(rStarFacC(i,j,bi,bj)**atm_kappa - factPI)
     &                  /(1. _d 0 -factPI)
     &                 + phi0surf(i,j,bi,bj)
          ENDIF
         ENDDO
        ENDDO
       ELSE
        DO j=jMin,jMax
         DO i=iMin,iMax
          IF (Ro_surf(i,j,bi,bj).EQ.rC(k)) THEN
           WRITE(msgBuf,'(3A)') 'CALC_GRAD_PHI_HYD: ',
     &      'Problem when Ro_surf=rC',
     &      ' with select_rStar,integr_GeoPot=1,4'
           CALL PRINT_ERROR( msgBuf , myThid)
           STOP 'CALC_GRAD_PHI_HYD: Pb in r* options implementation'
          ELSE
           varLoc(i,j) = phiHydC(i,j)
     &                  *(etaH(i,j,bi,bj)+Ro_surf(i,j,bi,bj)-rC(k))
     &                  /                (Ro_surf(i,j,bi,bj)-rC(k))
     &                 + phi0surf(i,j,bi,bj)
          ENDIF
         ENDDO
        ENDDO
       ENDIF
# endif /* DISABLE_RSTAR_CODE */
      ELSE
#else /* NONLIN_FRSURF */
      IF (.TRUE.) THEN
#endif /* NONLIN_FRSURF */
       DO j=jMin,jMax
        DO i=iMin,iMax
         varLoc(i,j) = phiHydC(i,j)+phi0surf(i,j,bi,bj)
        ENDDO
       ENDDO
      ENDIF

C--   Zonal & Meridional gradient of potential anomaly
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        dPhiHydX(i,j)  = 0. _d 0
        dPhiHydY(i,j)  = 0. _d 0
       ENDDO
      ENDDO
      DO j=jMin,jMax
       DO i=iMin+1,iMax
        dPhiHydX(i,j) = _recip_dxC(i,j,bi,bj)*recip_deepFacC(k)
     &                *( varLoc(i,j)-varLoc(i-1,j) )*recip_rhoFacC(k)
       ENDDO
      ENDDO
      DO j=jMin+1,jMax
       DO i=iMin,iMax
        dPhiHydY(i,j) = _recip_dyC(i,j,bi,bj)*recip_deepFacC(k)
     &                *( varLoc(i,j)-varLoc(i,j-1) )*recip_rhoFacC(k)
       ENDDO
      ENDDO

#ifdef NONLIN_FRSURF
      IF (select_rStar.GE.2 .AND. nonlinFreeSurf.GE.1 ) THEN
       IF ( buoyancyRelation .EQ. 'OCEANIC' ) THEN
C--    z* coordinate slope term: rho_prime/rho0 * Grad_r(g.z)
        factorZ = gravity*recip_rhoConst*recip_rhoFacC(k)*0.5 _d 0
        DO j=jMin,jMax
         DO i=iMin,iMax
          varLoc(i,j) = etaH(i,j,bi,bj)
     &                *(1. _d 0 + rC(k)*recip_Rcol(i,j,bi,bj))
         ENDDO
        ENDDO
        DO j=jMin,jMax
         DO i=iMin+1,iMax
          dPhiHydX(i,j) = dPhiHydX(i,j)
     &     +factorZ*(alphRho(i-1,j)+alphRho(i,j))
     &             *(varLoc(i,j)-varLoc(i-1,j))
     &             *recip_dxC(i,j,bi,bj)*recip_deepFacC(k)
         ENDDO
        ENDDO
        DO j=jMin+1,jMax
         DO i=iMin,iMax
          dPhiHydY(i,j) = dPhiHydY(i,j)
     &     +factorZ*(alphRho(i,j-1)+alphRho(i,j))
     &             *(varLoc(i,j)-varLoc(i,j-1))
     &             *recip_dyC(i,j,bi,bj)*recip_deepFacC(k)
         ENDDO
        ENDDO
       ELSEIF (buoyancyRelation .EQ. 'OCEANICP' ) THEN
C--    p* coordinate slope term: alpha_prime * Grad_r( p )
        factorP = 0.5 _d 0
        DO j=jMin,jMax
         DO i=iMin+1,iMax
          dPhiHydX(i,j) = dPhiHydX(i,j)
     &     +factorP*(alphRho(i-1,j)+alphRho(i,j))
     &             *(rStarFacC(i,j,bi,bj)-rStarFacC(i-1,j,bi,bj))
     &             *rC(k)*recip_dxC(i,j,bi,bj)*recip_deepFacC(k)
         ENDDO
        ENDDO
        DO j=jMin+1,jMax
         DO i=iMin,iMax
          dPhiHydY(i,j) = dPhiHydY(i,j)
     &     +factorP*(alphRho(i,j-1)+alphRho(i,j))
     &             *(rStarFacC(i,j,bi,bj)-rStarFacC(i,j-1,bi,bj))
     &             *rC(k)*recip_dyC(i,j,bi,bj)*recip_deepFacC(k)
         ENDDO
        ENDDO
       ELSEIF ( buoyancyRelation .EQ. 'ATMOSPHERIC' ) THEN
C--    p* coordinate slope term: alpha_prime * Grad_r( p )
        conv_theta2T = (rC(k)/atm_Po)**atm_kappa
        factorP = (atm_Rd/rC(k))*conv_theta2T*0.5 _d 0
        DO j=jMin,jMax
         DO i=iMin+1,iMax
          dPhiHydX(i,j) = dPhiHydX(i,j)
     &     +factorP*(alphRho(i-1,j)+alphRho(i,j))
     &             *(rStarFacC(i,j,bi,bj)-rStarFacC(i-1,j,bi,bj))
     &             *rC(k)*recip_dxC(i,j,bi,bj)*recip_deepFacC(k)
         ENDDO
        ENDDO
        DO j=jMin+1,jMax
         DO i=iMin,iMax
          dPhiHydY(i,j) = dPhiHydY(i,j)
     &     +factorP*(alphRho(i,j-1)+alphRho(i,j))
     &             *(rStarFacC(i,j,bi,bj)-rStarFacC(i,j-1,bi,bj))
     &             *rC(k)*recip_dyC(i,j,bi,bj)*recip_deepFacC(k)
         ENDDO
        ENDDO
       ENDIF
      ENDIF
#endif /* NONLIN_FRSURF */

#endif /* INCLUDE_PHIHYD_CALCULATION_CODE */

      RETURN
      END
