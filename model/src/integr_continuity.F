#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif
#undef PRESERVE_C69C_OUTPUT

CBOP
C     !ROUTINE: INTEGR_CONTINUITY
C     !INTERFACE:
      SUBROUTINE INTEGR_CONTINUITY(
     I                             uFld, vFld,
     I                             myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INTEGR_CONTINUITY
C     | o Integrate the continuity Eq : compute vertical velocity
C     |   and free surface "r-anomaly" (etaN,etaH) to satisfy
C     |   exactly the conservation of the Total Volume
C     | o Routine dummy_for_etan located here in order to print the
C     |   correct adjoint variable for etaN
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "SURFACE.h"
#include "FFIELDS.h"
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     uFld     :: Zonal velocity ( m/s )
C     vFld     :: Meridional velocity ( m/s )
C     myTime   :: Current time in simulation
C     myIter   :: Current iteration number in simulation
C     myThid   :: my Thread Id. number
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
C     bi,bj    :: tile index
C     i,j,k    :: Loop counters
C     uTrans   :: Volume transports ( uVel.xA )
C     vTrans   :: Volume transports ( vVel.yA )
C     hConv3d  :: horizontal transport convergence [wVel units, m/s]
C     hDivFlow :: Div. Barotropic Flow [wVel units, m/s]
      INTEGER bi, bj
      INTEGER i, j, k
      _RL uTrans(1:sNx+1,1:sNy+1)
      _RL vTrans(1:sNx+1,1:sNy+1)
      _RL hConv3d(1:sNx,1:sNy,Nr)
      _RL locFac
#ifdef EXACT_CONSERV
      INTEGER ks
      _RL hDivFlow(1:sNx,1:sNy)
      _RL facEmP
#endif /* EXACT_CONSERV */
#ifndef ALLOW_ADDFLUID
      _RL addMass(1)
#endif /* ndef ALLOW_ADDFLUID */
#if (defined NONLIN_FRSURF) && !(defined DISABLE_RSTAR_CODE)
      _RL rStarDhDt(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
# if (defined ALLOW_AUTODIFF_TAMC) && (defined ALLOW_DEPTH_CONTROL)
C     tkey :: tape key (tile dependent)
      INTEGER tkey
# endif
#else
      _RL rStarDhDt(1)
#endif
CEOP

#ifdef EXACT_CONSERV
       facEmP = 0.
       IF ( fluidIsWater.AND.useRealFreshWaterFlux ) facEmP=mass2rUnit
#endif /* EXACT_CONSERV */

C--   Start bi,bj loops
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef EXACT_CONSERV
C-    Initialise
        DO j=1,sNy
         DO i=1,sNx
           hDivFlow(i,j) = 0. _d 0
         ENDDO
        ENDDO
#endif /* EXACT_CONSERV */

C--   Compute the Convergence of horizontal Flow :
        DO k=1,Nr

C-    Calculate velocity field "volume transports" through tracer cell faces
C     anelastic: uTrans,vTrans are scaled by rhoFacC (~ mass transport).
         locFac = drF(k)*deepFacC(k)*rhoFacC(k)
         DO j=1,sNy+1
          DO i=1,sNx+1
           uTrans(i,j) = uFld(i,j,k,bi,bj)*_dyG(i,j,bi,bj)
     &                  *locFac*_hFacW(i,j,k,bi,bj)
           vTrans(i,j) = vFld(i,j,k,bi,bj)*_dxG(i,j,bi,bj)
     &                  *locFac*_hFacS(i,j,k,bi,bj)
          ENDDO
         ENDDO

C-    horizontal transport convergence [in wVel units]
         DO j=1,sNy
          DO i=1,sNx
           hConv3d(i,j,k) = -( uTrans(i+1,j) - uTrans(i,j)
     &                       + vTrans(i,j+1) - vTrans(i,j) )
          ENDDO
         ENDDO
         IF ( selectAddFluid.GE.1 .AND. myIter.NE.0 ) THEN
#ifdef ALLOW_ADDFLUID
          DO j=1,sNy
           DO i=1,sNx
            hConv3d(i,j,k) = ( hConv3d(i,j,k)
     &                       + addMass(i,j,k,bi,bj)*mass2rUnit
     &                       )
#ifndef PRESERVE_C69C_OUTPUT
     &                        *recip_rA(i,j,bi,bj)
#endif
           ENDDO
          ENDDO
#endif /* ALLOW_ADDFLUID */
#ifndef PRESERVE_C69C_OUTPUT
         ELSE
          DO j=1,sNy
           DO i=1,sNx
            hConv3d(i,j,k) = hConv3d(i,j,k)*recip_rA(i,j,bi,bj)
           ENDDO
          ENDDO
#endif
         ENDIF

#ifdef EXACT_CONSERV
         IF ( exactConserv ) THEN
C--   Compute the Divergence of The Barotropic Flow :
          DO j=1,sNy
           DO i=1,sNx
            hDivFlow(i,j) = hDivFlow(i,j)
     &                    - hConv3d(i,j,k)*maskC(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDIF
#endif /* EXACT_CONSERV */

#ifdef PRESERVE_C69C_OUTPUT
         DO j=1,sNy
          DO i=1,sNx
            hConv3d(i,j,k) = hConv3d(i,j,k)*recip_rA(i,j,bi,bj)
          ENDDO
         ENDDO
#endif

C-    End DO k=1,Nr
        ENDDO

#ifdef EXACT_CONSERV
        IF ( exactConserv ) THEN

#ifdef PRESERVE_C69C_OUTPUT
         DO j=1,sNy
          DO i=1,sNx
            hDivFlow(i,j) = hDivFlow(i,j)*recip_rA(i,j,bi,bj)
          ENDDO
         ENDDO
#endif

C------------------------------------
C note: deep-model not implemented for P-coordinate + realFreshWaterFlux ;
C       anelastic: always assumes that rhoFacF(1) = 1
         IF ( myIter.EQ.nIter0 .AND. myIter.NE.0
     &      .AND. fluidIsWater .AND. useRealFreshWaterFlux ) THEN

C     needs previous time-step value of E-P-R, that has not been loaded
C     and was not in old pickup-file ; try to use etaN & etaH instead.
          IF ( usePickupBeforeC54 ) THEN
           DO j=1,sNy
            DO i=1,sNx
             dEtaHdt(i,j,bi,bj) = (etaN(i,j,bi,bj)-etaH(i,j,bi,bj))
     &                          / (implicDiv2DFlow*deltaTFreeSurf)
            ENDDO
           ENDDO
          ENDIF

          DO j=1,sNy
           DO i=1,sNx
            PmEpR(i,j,bi,bj) = dEtaHdt(i,j,bi,bj)
     &                       + hDivFlow(i,j)*recip_deepFac2F(1)
            PmEpR(i,j,bi,bj) = PmEpR(i,j,bi,bj)*rUnit2mass
           ENDDO
          ENDDO
         ELSEIF ( myIter.EQ.nIter0 ) THEN
          DO j=1,sNy
           DO i=1,sNx
            ks = kSurfC(i,j,bi,bj)
            PmEpR(i,j,bi,bj) = 0. _d 0
            dEtaHdt(i,j,bi,bj) = -hDivFlow(i,j)*recip_deepFac2F(ks)
           ENDDO
          ENDDO
         ELSE
C--    Needs to get valid PmEpR (for T,S forcing) also in overlap regions
C       (e.g., if using KPP) => set over full index range
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            PmEpR(i,j,bi,bj) = -EmPmR(i,j,bi,bj)
           ENDDO
          ENDDO
          DO j=1,sNy
           DO i=1,sNx
            ks = kSurfC(i,j,bi,bj)
            dEtaHdt(i,j,bi,bj) = -hDivFlow(i,j)*recip_deepFac2F(ks)
     &                           -facEmP*EmPmR(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDIF
C------------------------------------

#ifdef ALLOW_OBCS
C--    reset dEtaHdt to zero outside the OB interior region
         IF ( useOBCS ) THEN
          DO j=1,sNy
           DO i=1,sNx
            dEtaHdt(i,j,bi,bj) = dEtaHdt(i,j,bi,bj)*maskInC(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDIF
#endif /* ALLOW_OBCS */

C-- end if exactConserv block
        ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

# if (defined NONLIN_FRSURF) && (!defined DISABLE_RSTAR_CODE)
#  if (defined ALLOW_AUTODIFF_TAMC) && (defined ALLOW_DEPTH_CONTROL)
        tkey = bi + (bj-1)*nSx + (ikey_dynamics-1)*nSx*nSy
CADJ STORE dEtaHdt(:,:,bi,bj) = comlev1_bibj, key = tkey, kind = isbyte
#  endif
        IF ( select_rStar.NE.0 ) THEN
C-- note: rStarDhDt is similar to rStarDhCDt from S/R CALC_R_STAR
C         except for deep-factor and rho factor.
         DO j=1,sNy
          DO i=1,sNx
           ks = kSurfC(i,j,bi,bj)
           rStarDhDt(i,j) = dEtaHdt(i,j,bi,bj)
     &                     *deepFac2F(ks)*rhoFacF(ks)
     &                     *recip_Rcol(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDIF
#  if (defined ALLOW_AUTODIFF_TAMC) && (defined ALLOW_DEPTH_CONTROL)
CADJ STORE rStarDhDt = comlev1_bibj, key = tkey, kind = isbyte
#  endif
# endif /* NONLIN_FRSURF and ndef DISABLE_RSTAR_CODE */

#endif /* EXACT_CONSERV */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Integrate continuity vertically for vertical velocity
        CALL INTEGRATE_FOR_W(
     I                        bi, bj,
     I                        hConv3d, rStarDhDt,
     O                        wVel,
     I                        myIter, myThid )

#ifdef ALLOW_OBCS
        IF ( useOBCS ) THEN
C--    reset W to zero outside the OB interior region
         DO k=1,Nr
          DO j=1,sNy
           DO i=1,sNx
            wVel(i,j,k,bi,bj) = wVel(i,j,k,bi,bj)*maskInC(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDDO
C--    Apply OBC to W (non-hydrostatic):
         IF ( nonHydrostatic )
     &        CALL OBCS_APPLY_W( bi, bj, 0, wVel, myThid )
        ENDIF
#endif /* ALLOW_OBCS */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   End bi,bj loops
       ENDDO
      ENDDO

#ifdef EXACT_CONSERV
C--   Update etaH (from etaN and dEtaHdt):
      IF ( exactConserv ) THEN
c       IF ( useRealFreshWaterFlux .AND. myIter.EQ.nIter0 )
c    &    _EXCH_XY_RL( PmEpR, myThid )
        CALL EXCH_XY_RL( dEtaHdt, myThid )
#ifdef ALLOW_DEBUG
        IF ( debugMode ) CALL DEBUG_CALL('UPDATE_ETAH',myThid)
#endif
        CALL UPDATE_ETAH( myTime, myIter, myThid )
      ENDIF
#endif /* EXACT_CONSERV */

#ifdef ALLOW_AUTODIFF_MONITOR
C     In reverse, print adjoint variable for etaN
      CALL DUMMY_FOR_ETAN( myTime, myIter, myThid )
      _EXCH_XY_RL( etaN , myThid )
#endif /* ALLOW_AUTODIFF_MONITOR */

      IF ( implicitIntGravWave .OR. myIter.EQ.nIter0 )
     &    _EXCH_XYZ_RL( wVel , myThid )

#ifdef NONLIN_FRSURF
# ifndef DISABLE_SIGMA_CODE
      IF ( nonlinFreeSurf.GT.0 .AND. selectSigmaCoord.NE.0 ) THEN
        CALL UPDATE_ETAWS( myTime, myIter, myThid )
      ENDIF
# endif /* DISABLE_SIGMA_CODE */
#endif /* NONLIN_FRSURF */

      RETURN
      END
