C $Header: /u/gcmpack/MITgcm/model/src/timestep.F,v 1.52 2010/09/11 21:27:13 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#ifdef ALLOW_CD_CODE
#include "CD_CODE_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: TIMESTEP
C     !INTERFACE:
      SUBROUTINE TIMESTEP( bi, bj, iMin, iMax, jMin, jMax, k,
     I                     dPhiHydX,dPhiHydY, phiSurfX, phiSurfY,
     I                     guDissip, gvDissip,
     I                     myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R TIMESTEP
C     | o Step model fields forward in time
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
#include "RESTART.h"
#include "DYNVARS.h"
#ifdef ALLOW_NONHYDROSTATIC
#include "NH_VARS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     dPhiHydX,Y :: Gradient (X & Y directions) of Hydrostatic Potential
C     phiSurfX :: gradient of Surface potential (Pressure/rho, ocean)
C     phiSurfY ::          or geopotential (atmos) in X and Y direction
C     guDissip :: dissipation tendency (all explicit terms), u component
C     gvDissip :: dissipation tendency (all explicit terms), v component

      INTEGER bi,bj,iMin,iMax,jMin,jMax
      INTEGER k
      _RL     dPhiHydX(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL     dPhiHydY(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL     phiSurfX(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     phiSurfY(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     guDissip(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     gvDissip(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     myTime
      INTEGER myIter, myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j
      _RL phxFac,phyFac, psFac
      _RL     gUtmp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     gVtmp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#ifdef ALLOW_NONHYDROSTATIC
      _RL     nhFac
#endif
#ifdef ALLOW_DIAGNOSTICS
C    Allow diagnosis of external forcing
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON
#endif
      LOGICAL externForcDiagIsOn
#if (defined ALLOW_DIAGNOSTICS) || (defined CD_CODE_NO_AB_MOMENTUM)
      _RL     gUext(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     gVext(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif
#ifdef ALLOW_CD_CODE
      _RL     guCor(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     gvCor(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif
CEOP

C-- explicit part of the surface potential gradient is added in this S/R
      psFac = pfFacMom*(1. _d 0 - implicSurfPress)
     &       *recip_deepFacC(k)*recip_rhoFacC(k)

C--  factors for gradient (X & Y directions) of Hydrostatic Potential
      phxFac = pfFacMom
      phyFac = pfFacMom

      externForcDiagIsOn = useDiagnostics .AND. momForcing
#ifdef ALLOW_DIAGNOSTICS
      IF ( externForcDiagIsOn ) THEN
        externForcDiagIsOn = DIAGNOSTICS_IS_ON('Um_Ext  ',myThid)
     &                  .OR. DIAGNOSTICS_IS_ON('Vm_Ext  ',myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-    Initialize local arrays (not really necessary but safer)
      DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
          gUtmp(i,j) = 0. _d 0
          gVtmp(i,j) = 0. _d 0
#ifdef ALLOW_CD_CODE
          guCor(i,j) = 0. _d 0
          gvCor(i,j) = 0. _d 0
#endif
        ENDDO
      ENDDO

      IF ( .NOT.staggerTimeStep .AND. .NOT. implicitIntGravWave ) THEN
C--   Synchronous time step: add grad Phi_Hyp to gU,gV before doing Adams-Bashforth
        DO j=jMin,jMax
         DO i=iMin,iMax
           gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj) - phxFac*dPhiHydX(i,j)
           gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj) - phyFac*dPhiHydY(i,j)
         ENDDO
        ENDDO
        phxFac = 0.
        phyFac = 0.
c     ELSE
C--   Stagger time step: grad Phi_Hyp will be added later
      ENDIF

C--   Dissipation term inside the Adams-Bashforth:
      IF ( momViscosity .AND. momDissip_In_AB) THEN
        DO j=jMin,jMax
         DO i=iMin,iMax
           gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj) + guDissip(i,j)
           gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj) + gvDissip(i,j)
         ENDDO
        ENDDO
      ENDIF

C--   Forcing term inside the Adams-Bashforth:
      IF ( momForcing .AND. momForcingOutAB.NE.1 ) THEN
#ifdef ALLOW_DIAGNOSTICS
        IF ( externForcDiagIsOn ) THEN
         DO j=1,sNy+1
          DO i=1,sNx+1
           gUext(i,j) = gU(i,j,k,bi,bj)
           gVext(i,j) = gV(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDIF
#endif /* ALLOW_DIAGNOSTICS */

        CALL EXTERNAL_FORCING_U(
     I     iMin,iMax,jMin,jMax,bi,bj,k,
     I     myTime,myThid)
        CALL EXTERNAL_FORCING_V(
     I     iMin,iMax,jMin,jMax,bi,bj,k,
     I     myTime,myThid)

#ifdef ALLOW_DIAGNOSTICS
        IF ( externForcDiagIsOn ) THEN
         DO j=1,sNy+1
          DO i=1,sNx+1
           gUext(i,j) = gU(i,j,k,bi,bj)-gUext(i,j)
           gVext(i,j) = gV(i,j,k,bi,bj)-gVext(i,j)
          ENDDO
         ENDDO
        ENDIF
#endif /* ALLOW_DIAGNOSTICS */
      ENDIF

#ifdef CD_CODE_NO_AB_MOMENTUM
      IF (useCDscheme) THEN
C-    CD-scheme, before doing AB, store gU,Vtmp = gU,V^n (+dissip. +forcing)
        DO j=jMin,jMax
         DO i=iMin,iMax
           gUtmp(i,j) = gU(i,j,k,bi,bj)
           gVtmp(i,j) = gV(i,j,k,bi,bj)
         ENDDO
        ENDDO
      ENDIF
#endif /* CD_CODE_NO_AB_MOMENTUM */

C-    Compute effective gU,gV_[n+1/2] terms (including Adams-Bashforth weights)
C     and save gU,gV_[n] into guNm1,gvNm1 for the next time step.
#ifdef ALLOW_ADAMSBASHFORTH_3
      CALL ADAMS_BASHFORTH3(
     I                        bi, bj, k,
     U                        gU, guNm,
     I                        mom_StartAB, myIter, myThid )
      CALL ADAMS_BASHFORTH3(
     I                        bi, bj, k,
     U                        gV, gvNm,
     I                        mom_StartAB, myIter, myThid )
#else /* ALLOW_ADAMSBASHFORTH_3 */
      CALL ADAMS_BASHFORTH2(
     I                        bi, bj, k,
     U                        gU, guNm1,
     I                        mom_StartAB, myIter, myThid )
      CALL ADAMS_BASHFORTH2(
     I                        bi, bj, k,
     U                        gV, gvNm1,
     I                        mom_StartAB, myIter, myThid )
#endif /* ALLOW_ADAMSBASHFORTH_3 */

C--   Forcing term outside the Adams-Bashforth:
      IF ( momForcing .AND. momForcingOutAB.EQ.1 ) THEN

#if (defined ALLOW_DIAGNOSTICS) || (defined CD_CODE_NO_AB_MOMENTUM)
        IF ( externForcDiagIsOn .OR. useCDscheme ) THEN
         DO j=jMin,jMax
          DO i=iMin,iMax
           gUext(i,j) = gU(i,j,k,bi,bj)
           gVext(i,j) = gV(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDIF
#endif /* ALLOW_DIAGNOSTICS or CD_CODE_NO_AB_MOMENTUM */

        CALL EXTERNAL_FORCING_U(
     I     iMin,iMax,jMin,jMax,bi,bj,k,
     I     myTime,myThid)
        CALL EXTERNAL_FORCING_V(
     I     iMin,iMax,jMin,jMax,bi,bj,k,
     I     myTime,myThid)

#if (defined ALLOW_DIAGNOSTICS) || (defined CD_CODE_NO_AB_MOMENTUM)
        IF ( externForcDiagIsOn .OR. useCDscheme ) THEN
         DO j=jMin,jMax
          DO i=iMin,iMax
           gUext(i,j) = gU(i,j,k,bi,bj)-gUext(i,j)
           gVext(i,j) = gV(i,j,k,bi,bj)-gVext(i,j)
          ENDDO
         ENDDO
        ENDIF
#endif /* ALLOW_DIAGNOSTICS or CD_CODE_NO_AB_MOMENTUM */

C--   Forcing term outside the Adams-Bashforth: end of if block
      ENDIF

#ifdef ALLOW_CD_CODE
      IF (useCDscheme) THEN
#ifdef CD_CODE_NO_AB_MOMENTUM
C-     CD-scheme, add forcing to gU,Vtmp = gU,V^n (+ dissip.)
        IF ( momForcing .AND. momForcingOutAB.EQ.1 ) THEN
         DO j=jMin,jMax
          DO i=iMin,iMax
           gUtmp(i,j) = gUtmp(i,j) + guExt(i,j)
           gVtmp(i,j) = gVtmp(i,j) + gvExt(i,j)
          ENDDO
         ENDDO
        ENDIF
#else /* CD_CODE_NO_AB_MOMENTUM */
C-     CD-scheme, store gU,Vtmp = gU,V^n+1/2 (+dissip.) +forcing
        DO j=jMin,jMax
         DO i=iMin,iMax
           gUtmp(i,j) = gU(i,j,k,bi,bj)
           gVtmp(i,j) = gV(i,j,k,bi,bj)
         ENDDO
        ENDDO
#endif /* CD_CODE_NO_AB_MOMENTUM */
        IF ( momViscosity .AND. .NOT.momDissip_In_AB ) THEN
C-     CD-scheme, add dissip. to gU,Vtmp = gU,V  + forcing
         DO j=jMin,jMax
          DO i=iMin,iMax
           gUtmp(i,j) = gUtmp(i,j) + guDissip(i,j)
           gVtmp(i,j) = gVtmp(i,j) + gvDissip(i,j)
          ENDDO
         ENDDO
        ENDIF
C-     Step forward D-grid velocity using C-grid gU,Vtmp = gU,V +dissip +forcing
C      and return coriolis terms on C-grid (guCor,gvCor)
        CALL CD_CODE_SCHEME(
     I                  bi,bj,k, dPhiHydX,dPhiHydY, gUtmp,gVtmp,
     O                  guCor,gvCor,
     I                  myTime, myIter, myThid)
        DO j=jMin,jMax
         DO i=iMin,iMax
           gUtmp(i,j) = gU(i,j,k,bi,bj)
     &                + guCor(i,j)
           gVtmp(i,j) = gV(i,j,k,bi,bj)
     &                + gvCor(i,j)
         ENDDO
        ENDDO
      ELSE
#endif /* ALLOW_CD_CODE */
        DO j=jMin,jMax
         DO i=iMin,iMax
           gUtmp(i,j) = gU(i,j,k,bi,bj)
           gVtmp(i,j) = gV(i,j,k,bi,bj)
         ENDDO
        ENDDO
#ifdef ALLOW_CD_CODE
      ENDIF
#endif /* ALLOW_CD_CODE */

#ifdef NONLIN_FRSURF
      IF ( .NOT. vectorInvariantMomentum
     &     .AND. nonlinFreeSurf.GT.1 ) THEN
       IF ( select_rStar.GT.0 ) THEN
# ifndef DISABLE_RSTAR_CODE
        DO j=jMin,jMax
         DO i=iMin,iMax
           gUtmp(i,j) = gUtmp(i,j)/rStarExpW(i,j,bi,bj)
           gVtmp(i,j) = gVtmp(i,j)/rStarExpS(i,j,bi,bj)
         ENDDO
        ENDDO
# endif /* DISABLE_RSTAR_CODE */
       ELSEIF ( selectSigmaCoord.NE.0 ) THEN
# ifndef DISABLE_SIGMA_CODE
        DO j=jMin,jMax
         DO i=iMin,iMax
           gUtmp(i,j) = gUtmp(i,j)
     &        /( 1. _d 0 + dEtaWdt(i,j,bi,bj)*deltaTfreesurf
     &                    *dBHybSigF(k)*recip_drF(k)
     &                    *recip_hFacW(i,j,k,bi,bj)
     &         )
           gVtmp(i,j) = gVtmp(i,j)
     &        /( 1. _d 0 + dEtaSdt(i,j,bi,bj)*deltaTfreesurf
     &                    *dBHybSigF(k)*recip_drF(k)
     &                    *recip_hFacS(i,j,k,bi,bj)
     &         )
         ENDDO
        ENDDO
# endif /* DISABLE_SIGMA_CODE */
       ELSE
        DO j=jMin,jMax
         DO i=iMin,iMax
          IF ( k.EQ.kSurfW(i,j,bi,bj) ) THEN
           gUtmp(i,j) = gUtmp(i,j)
     &         *_hFacW(i,j,k,bi,bj)/hFac_surfW(i,j,bi,bj)
          ENDIF
          IF ( k.EQ.kSurfS(i,j,bi,bj) ) THEN
           gVtmp(i,j) = gVtmp(i,j)
     &         *_hFacS(i,j,k,bi,bj)/hFac_surfS(i,j,bi,bj)
          ENDIF
         ENDDO
        ENDDO
       ENDIF
      ENDIF
#endif /* NONLIN_FRSURF */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Dissipation term outside the Adams-Bashforth:
      IF ( momViscosity .AND. .NOT.momDissip_In_AB ) THEN
        DO j=jMin,jMax
         DO i=iMin,iMax
          gUtmp(i,j) = gUtmp(i,j) + guDissip(i,j)
          gVtmp(i,j) = gVtmp(i,j) + gvDissip(i,j)
         ENDDO
        ENDDO
      ENDIF

#ifdef ALLOW_NONHYDROSTATIC
C-- explicit part of the NH pressure gradient is added here
      IF ( use3Dsolver .AND. implicitNHPress.NE.1. _d 0 ) THEN
        nhFac = pfFacMom*(1. _d 0 - implicitNHPress)
     &         *recip_deepFacC(k)*recip_rhoFacC(k)
       IF ( exactConserv ) THEN
        DO j=jMin,jMax
         DO i=iMin,iMax
          gUtmp(i,j) = gUtmp(i,j)
     &               - nhFac*_recip_dxC(i,j,bi,bj)*
     &                ( (phi_nh(i,j,k,bi,bj)-phi_nh(i-1,j,k,bi,bj))
     &                 -( dPhiNH(i,j,bi,bj) - dPhiNH(i-1,j,bi,bj) )
     &                )
          gVtmp(i,j) = gVtmp(i,j)
     &               - nhFac*_recip_dyC(i,j,bi,bj)*
     &                ( (phi_nh(i,j,k,bi,bj)-phi_nh(i,j-1,k,bi,bj))
     &                 -( dPhiNH(i,j,bi,bj) - dPhiNH(i,j-1,bi,bj) )
     &                )
         ENDDO
        ENDDO
       ELSE
        DO j=jMin,jMax
         DO i=iMin,iMax
          gUtmp(i,j) = gUtmp(i,j)
     &               - nhFac*_recip_dxC(i,j,bi,bj)*
     &                  (phi_nh(i,j,k,bi,bj)-phi_nh(i-1,j,k,bi,bj))
          gVtmp(i,j) = gVtmp(i,j)
     &               - nhFac*_recip_dyC(i,j,bi,bj)*
     &                  (phi_nh(i,j,k,bi,bj)-phi_nh(i,j-1,k,bi,bj))
         ENDDO
        ENDDO
       ENDIF
      ENDIF
#endif /* ALLOW_NONHYDROSTATIC */

C     Step forward zonal velocity (store in Gu)
      DO j=jMin,jMax
        DO i=iMin,iMax
          gU(i,j,k,bi,bj) = uVel(i,j,k,bi,bj)
     &     +deltaTmom*(
     &         gUtmp(i,j)
     &       - psFac*phiSurfX(i,j)
     &       - phxFac*dPhiHydX(i,j)
     &        )*_maskW(i,j,k,bi,bj)
        ENDDO
      ENDDO

C     Step forward meridional velocity (store in Gv)
      DO j=jMin,jMax
        DO i=iMin,iMax
          gV(i,j,k,bi,bj) = vVel(i,j,k,bi,bj)
     &     +deltaTmom*(
     &         gVtmp(i,j)
     &       - psFac*phiSurfY(i,j)
     &       - phyFac*dPhiHydY(i,j)
     &        )*_maskS(i,j,k,bi,bj)
        ENDDO
      ENDDO

#ifdef ALLOW_DIAGNOSTICS
      IF ( externForcDiagIsOn ) THEN
        CALL DIAGNOSTICS_FILL( gUext,'Um_Ext  ',k,1,2,bi,bj,myThid )
        CALL DIAGNOSTICS_FILL( gVext,'Vm_Ext  ',k,1,2,bi,bj,myThid )
      ENDIF
#ifdef ALLOW_CD_CODE
      IF ( useCDscheme .AND. useDiagnostics ) THEN
        CALL DIAGNOSTICS_FILL( guCor,'Um_Cori ',k,1,2,bi,bj,myThid )
        CALL DIAGNOSTICS_FILL( gvCor,'Vm_Cori ',k,1,2,bi,bj,myThid )
      ENDIF
#endif /* ALLOW_CD_CODE */
#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
